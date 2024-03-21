with Ada.Text_IO;
with Text_IO; use Text_IO;
with Interfaces.C; use Interfaces.C;
with Raylib; use Raylib;
with Raymath; use Raymath;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Hashed_Maps;
use Ada.Containers;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Interfaces.C.Pointers;
with Ada.Unchecked_Conversion;

procedure Game is
    package Random_Integer is
        new Ada.Numerics.Discrete_Random(Result_Subtype => Integer);

    Gen: Random_Integer.Generator;
    DEVELOPMENT : constant Boolean := True;

    type Palette is (
      COLOR_BACKGROUND,
      COLOR_FLOOR,
      COLOR_WALL,
      COLOR_BARRICADE,
      COLOR_PLAYER,
      COLOR_DOOR_KEY,
      COLOR_BOMB,
      COLOR_LABEL,
      COLOR_GUARD,
      COLOR_URMOM,
      COLOR_GNOME,
      COLOR_CHECKPOINT,
      COLOR_EXPLOSION,
      COLOR_HEALTHBAR,
      COLOR_NEW_GAME,
      COLOR_EYES,
      COLOR_FINAL);

    type Byte is mod 256;
    type HSV_Comp is (Hue, Sat, Value);
    type HSV is array (HSV_Comp) of Byte;

    function HSV_To_RGB(C: HSV) return Color is
        H: constant C_Float := C_Float(C(Hue))/255.0*360.0;
        S: constant C_Float := C_Float(C(Sat))/255.0;
        V: constant C_Float := C_Float(C(Value))/255.0;
    begin
        return Color_From_HSV(H, S, V);
    end;

    Palette_RGB: array (Palette) of Color := [others => (A => 255, others => 0)];
    Palette_HSV: array (Palette) of HSV := [others => [others => 0]];

    package Double_IO is new Ada.Text_IO.Float_IO(Double);

    procedure Save_Colors(File_Name: String) is
        F: File_Type;
    begin
        Create(F, Out_File, File_Name);
        for C in Palette loop
            Put(F, C'Image);
            for Comp in HSV_Comp loop
                Put(F, Palette_HSV(C)(Comp)'Image);
            end loop;
            Put_Line(F, "");
        end loop;
        Close(F);
    end;

    procedure Load_Colors(File_Name: String) is
        F: File_Type;
        Line_Number : Integer := 0;
    begin
        Open(F, In_File, File_Name);
        while not End_Of_File(F) loop
            Line_Number := Line_Number + 1;
            declare
                Line: Unbounded_String := To_Unbounded_String(Get_Line(F));

                function Chop_By(Src: in out Unbounded_String; Pattern: String) return Unbounded_String is
                    Space_Index: constant Integer := Index(Src, Pattern);
                    Result: Unbounded_String;
                begin
                    if Space_Index = 0 then
                        Result := Src;
                        Src := Null_Unbounded_String;
                    else
                        Result := Unbounded_Slice(Src, 1, Space_Index - 1);
                        Src := Unbounded_Slice(Src, Space_Index + 1, Length(Src));
                    end if;

                    return Result;
                end;
                function Find_Color_By_Key(Key: Unbounded_String; Co: out Palette) return Boolean is
                begin
                    for C in Palette loop
                        if Key = C'Image then
                            Co := C;
                            return True;
                        end if;
                    end loop;
                    return False;
                end;
                C: Palette;
                Key: constant Unbounded_String := Chop_By(Line, " ");
            begin
                Line := Trim(Line, Ada.Strings.Left);
                if Find_Color_By_Key(Key, C) then
                    Line := Trim(Line, Ada.Strings.Left);
                    Palette_HSV(C)(Hue) := Byte'Value(To_String(Chop_By(Line, " ")));
                    Line := Trim(Line, Ada.Strings.Left);
                    Palette_HSV(C)(Sat) := Byte'Value(To_String(Chop_By(Line, " ")));
                    Line := Trim(Line, Ada.Strings.Left);
                    Palette_HSV(C)(Value) := Byte'Value(To_String(Chop_By(Line, " ")));
                    Palette_RGB(C) := Color_From_HSV(C_Float(Palette_HSV(C)(Hue))/255.0*360.0, C_Float(Palette_HSV(C)(Sat))/255.0, C_Float(Palette_HSV(C)(Value))/255.0);
                else
                    Put_Line(File_Name & ":" & Line_Number'Image & "WARNING: Unknown Palette Color: """ & To_String(Key) & """");
                end if;
            end;
        end loop;
        Close(F);
    exception
        when E: Name_Error =>
            Put_Line("WARNING: could not load colors from file " & File_Name & ": " & Exception_Message(E));
    end;

    TURN_DURATION_SECS      : constant Float := 0.125;
    GUARD_ATTACK_COOLDOWN   : constant Integer := 10;
    BOSS_EXPLOSION_DAMAGE  : constant Float := 0.45;
    GUARD_TURN_REGENERATION : constant Float := 0.01;
    BOMB_GENERATOR_COOLDOWN : constant Integer := 10;
    GUARD_STEPS_LIMIT       : constant Integer := 4;
    GUARD_STEP_LENGTH_LIMIT : constant Integer := 100;
    EXPLOSION_LENGTH        : constant Integer := 10;

    type IVector2 is record
        X, Y: Integer;
    end record;

    type Cell is (None, Floor, Wall, Barricade, Door, Explosion);
    Cell_Size : constant Vector2 := (x => 50.0, y => 50.0);

    function Cell_Colors(C: Cell) return Color is
    begin
        case C is
            when None      => return Palette_RGB(COLOR_BACKGROUND);
            when Floor     => return Palette_RGB(COLOR_FLOOR);
            when Wall      => return Palette_RGB(COLOR_WALL);
            when Barricade => return Palette_RGB(COLOR_BARRICADE);
            when Door      => return Palette_RGB(COLOR_DOOR_KEY);
            when Explosion => return Palette_RGB(COLOR_EXPLOSION);
        end case;
    end;

    type Path_Map is array (Positive range <>, Positive range <>) of Integer;
    type Path_Map_Access is access Path_Map;
    procedure Delete_Path_Map is new Ada.Unchecked_Deallocation(Path_Map, Path_Map_Access);
    type Map is array (Positive range <>, Positive range <>) of Cell;
    type Map_Access is access Map;
    procedure Delete_Map is new Ada.Unchecked_Deallocation(Map, Map_Access);

    function "<="(A, B: IVector2) return Boolean is
    begin
        return A.X <= B.X and then A.Y <= B.Y;
    end;

    function "<"(A, B: IVector2) return Boolean is
    begin
        return A.X < B.X and then A.Y < B.Y;
    end;

    function "="(A, B: IVector2) return Boolean is
    begin
        return A.X = B.X and then A.Y = B.Y;
    end;

    function "+"(A, B: IVector2) return IVector2 is
    begin
        return (A.X + B.X, A.Y + B.Y);
    end;

    function "-"(A, B: IVector2) return IVector2 is
    begin
        return (A.X - B.X, A.Y - B.Y);
    end;

    function Equivalent_IVector2(Left, Right: IVector2) return Boolean is
    begin
        return Left.X = Right.X and then Left.Y = Right.Y;
    end;

    function Hash_IVector2(V: IVector2) return Hash_Type is
        M31: constant Hash_Type := 2**31-1; -- a nice Mersenne prime
    begin
        return Hash_Type(V.X) * M31 + Hash_Type(V.Y);
    end;

    type Item_Kind is (Key, Bomb_Gen, Checkpoint, New_Game);

    type Item(Kind: Item_Kind := Key) is record
        case Kind is
            when Bomb_Gen =>
                Cooldown: Integer;
            when others => null;
        end case;
    end record;

    package Hashed_Map_Items is new
        Ada.Containers.Hashed_Maps(
            Key_Type => IVector2,
            Element_Type => Item,
            Hash => Hash_IVector2,
            Equivalent_Keys => Equivalent_IVector2);

    function To_Vector2(iv: IVector2) return Vector2 is
    begin
        return (X => C_float(iv.X), Y => C_float(iv.Y));
    end;

    type Player_State is record
        Prev_Position: IVector2;
        Position: IVector2;
        Keys: Integer := 0;
        Bombs: Integer := 1;
        Bomb_Slots: Integer := 1;
        Dead: Boolean := False;
    end record;

    type Eyes_Kind is (Eyes_Open, Eyes_Closed, Eyes_Angry);
    type Eye_Mesh is new Vector2_Array(1..4);
    type Eye is (Left_Eye, Right_Eye);
    type Eyes_Mesh is array (Eye) of Eye_Mesh;
    Eyes_Meshes: constant array (Eyes_Kind) of Eyes_Mesh := [
        Eyes_Open => [
            Left_Eye => [ (0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0) ],
            Right_Eye => [ (0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0) ]
        ],
        Eyes_Closed => [
            Left_Eye => [ (0.0, 0.8), (0.0, 1.0), (1.0, 0.8), (1.0, 1.0) ],
            Right_Eye => [ (0.0, 0.8), (0.0, 1.0), (1.0, 0.8), (1.0, 1.0) ]
        ],
        Eyes_Angry => [
            Left_Eye => [ (0.0, 0.0), (0.0, 1.0), (1.0, 0.3), (1.0, 1.0) ],
            Right_Eye => [ (0.0, 0.3), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0) ]
        ]
    ];

    type Eeper_Kind is (Eeper_Guard, Eeper_Urmom, Eeper_Gnome, Eeper_Final);

    type Eeper_State is record
        Kind: Eeper_Kind;
        Dead: Boolean := True;
        Position, Prev_Position: IVector2;
        Prev_Eyes: Eyes_Kind;
        Eyes: Eyes_Kind := Eyes_Closed;
        Size: IVector2;
        Path: Path_Map_Access;

        Background: Palette;
        Health: Float := 1.0;
        Attack_Cooldown: Integer := GUARD_ATTACK_COOLDOWN;
    end record;

    type Bomb_State is record
        Position: IVector2;
        Countdown: Integer := 0;
    end record;

    type Bomb_State_Array is array (1..10) of Bomb_State;

    type Eeper_Index is range 1..10;
    type Eeper_Array is array (Eeper_Index) of Eeper_State;

    type Checkpoint_State is record
        Map: Map_Access := Null;
        Player_Position: IVector2;
        Player_Keys: Integer;
        Player_Bombs: Integer;
        Player_Bomb_Slots: Integer;
        Eepers: Eeper_Array;
        Items: Hashed_Map_Items.Map;
    end record;

    type Game_State is record
        Map: Map_Access := Null;
        Player: Player_State;
        Eepers: Eeper_Array;

        Turn_Animation: Float := 0.0;

        Items: Hashed_Map_Items.Map;
        Bombs: Bomb_State_Array;
        Camera_Position: Vector2 := (x => 0.0, y => 0.0);
        Camera_Velocity: Vector2 := (x => 0.0, y => 0.0);

        Checkpoint: Checkpoint_State;

        Duration_Of_Last_Turn: Double;
    end record;

    function Within_Map(Game: Game_State; Position: IVector2) return Boolean is
    begin
        return Position.Y in Game.Map'Range(1) and then Position.X in Game.Map'Range(2);
    end;

    function Clone_Map(M0: Map_Access) return Map_Access is
        M1: Map_Access;
    begin
        M1 := new Map(M0'Range(1), M0'Range(2));
        M1.all := M0.all;
        return M1;
    end;

    type Direction is (Left, Right, Up, Down);

    Direction_Vector: constant array (Direction) of IVector2 := [
      Left  => (X => -1, Y => 0),
      Right => (X => 1, Y => 0),
      Up    => (X => 0, Y => -1),
      Down  => (X => 0, Y => 1)];

    function Inside_Of_Rect(Start, Size, Point: in IVector2) return Boolean is
    begin
        return Start <= Point and then Point < Start + Size;
    end;

    function Eeper_Can_Stand_Here(Game: Game_State; Start: IVector2; Me: Eeper_Index) return Boolean is
        Size: constant IVector2 := Game.Eepers(Me).Size;
    begin
        for X in Start.X..Start.X+Size.X-1 loop
            for Y in Start.Y..Start.Y+Size.Y-1 loop
                if not Within_Map(Game, (X, Y)) then
                    return False;
                end if;
                if Game.Map(Y, X) /= Floor then
                    return False;
                end if;
                for Index in Eeper_Index loop
                    if not Game.Eepers(Index).Dead and then Index /= Me then
                        declare
                            Eeper : constant Eeper_State := Game.Eepers(Index);
                        begin
                            if Inside_Of_Rect(Eeper.Position, Eeper.Size, (X, Y)) then
                                return False;
                            end if;
                        end;
                    end if;
                end loop;
            end loop;
        end loop;
        return True;
    end;

    package Queue is new
      Ada.Containers.Vectors(Index_Type => Natural, Element_Type => IVector2);

    procedure Recompute_Path_For_Eeper
      (Game: in out Game_State;
       Me: Eeper_Index;
       Steps_Limit: Integer;
       Step_Length_Limit: Integer;
       Stop_At_Me: Boolean := True)
    is
        Q: Queue.Vector;
    begin
        for Y in Game.Eepers(Me).Path'Range(1) loop
            for X in Game.Eepers(Me).Path'Range(2) loop
                Game.Eepers(Me).Path(Y, X) := -1;
            end loop;
        end loop;

        for Dy in 0..Game.Eepers(Me).Size.Y-1 loop
            for Dx in 0..Game.Eepers(Me).Size.X-1 loop
                declare
                    Position: constant IVector2 := Game.Player.Position - (Dx, Dy);
                begin
                    if Eeper_Can_Stand_Here(Game, Position, Me) then
                        Game.Eepers(Me).Path(Position.Y, Position.X) := 0;
                        Q.Append(Position);
                    end if;
                end;
            end loop;
        end loop;

        while not Q.Is_Empty loop
            declare
                Position: constant IVector2 := Q(0);
            begin
                Q.Delete_First;

                if Stop_At_Me and then Position = Game.Eepers(Me).Position then
                    exit;
                end if;

                if Game.Eepers(Me).Path(Position.Y, Position.X) >= Steps_Limit then
                    exit;
                end if;

                for Dir in Direction loop
                    declare
                        New_Position: IVector2 := Position + Direction_Vector(Dir);
                    begin
                        for Limit in 1..Step_Length_Limit loop
                            if not Eeper_Can_Stand_Here(Game, New_Position, Me) then
                                exit;
                            end if;
                            if Game.Eepers(Me).Path(New_Position.Y, New_Position.X) >= 0 then
                                exit;
                            end if;
                            Game.Eepers(Me).Path(New_Position.Y, New_Position.X) := Game.Eepers(Me).Path(Position.Y, Position.X) + 1;
                            Q.Append(New_Position);
                            New_Position := New_Position + Direction_Vector(Dir);
                        end loop;
                    end;
                end loop;
            end;
        end loop;
    end;

    procedure Game_Save_Checkpoint(Game: in out Game_State) is
    begin
        if Game.Checkpoint.Map /= null then
            Delete_Map(Game.Checkpoint.Map);
        end if;
        Game.Checkpoint.Map               := Clone_Map(Game.Map);
        Game.Checkpoint.Player_Position   := Game.Player.Position;
        Game.Checkpoint.Player_Keys       := Game.Player.Keys;
        Game.Checkpoint.Player_Bombs      := Game.Player.Bombs;
        Game.Checkpoint.Player_Bomb_Slots := Game.Player.Bomb_Slots;
        Game.Checkpoint.Eepers            := Game.Eepers;
        Game.Checkpoint.Items             := Game.Items;
    end;

    procedure Game_Restore_Checkpoint(Game: in out Game_State) is
    begin
        if Game.Map /= null then
            Delete_Map(Game.Map);
        end if;
        Game.Map               := Clone_Map(Game.Checkpoint.Map);
        Game.Player.Position   := Game.Checkpoint.Player_Position;
        Game.Player.Keys       := Game.Checkpoint.Player_Keys;
        Game.Player.Bombs      := Game.Checkpoint.Player_Bombs;
        Game.Player.Bomb_Slots := Game.Checkpoint.Player_Bomb_Slots;
        Game.Eepers            := Game.Checkpoint.Eepers;
        Game.Items             := Game.Checkpoint.Items;
    end;

    procedure Spawn_Gnome(Game: in out Game_State; Position: IVector2) is
    begin
        for Eeper of Game.Eepers loop
            if Eeper.Dead then
                  Eeper.Kind := Eeper_Gnome;
                  Eeper.Dead := False;
                  Eeper.Background := COLOR_GNOME;
                  Eeper.Position := Position;
                  Eeper.Prev_Position := Position;
                  Eeper.Size := (1, 1);
                exit;
            end if;
        end loop;
    end;

    procedure Spawn_Final(Game: in out Game_State; Position: IVector2) is
    begin
        for Eeper of Game.Eepers loop
            if Eeper.Dead then
                  Eeper.Kind := Eeper_Final;
                  Eeper.Dead := False;
                  Eeper.Background := COLOR_FINAL;
                  Eeper.Position := Position;
                  Eeper.Prev_Position := Position;
                  Eeper.Health := 1.0;
                  Eeper.Size := (7, 7);
                exit;
            end if;
        end loop;
    end;


    procedure Spawn_Urmom(Game: in out Game_State; Position: IVector2) is
    begin
        for Eeper of Game.Eepers loop
            if Eeper.Dead then
                  Eeper.Kind := Eeper_Urmom;
                  Eeper.Dead := False;
                  Eeper.Background := COLOR_URMOM;
                  Eeper.Position := Position;
                  Eeper.Prev_Position := Position;
                  Eeper.Health := 1.0;
                  Eeper.Size := (7, 7);
                exit;
            end if;
        end loop;
    end;

    procedure Spawn_Guard(Game: in out Game_State; Position: IVector2) is
    begin
        for Eeper of Game.Eepers loop
            if Eeper.Dead then
                Eeper.Kind := Eeper_Guard;
                Eeper.Background := COLOR_GUARD;
                Eeper.Dead := False;
                Eeper.Position := Position;
                Eeper.Prev_Position := Position;
                Eeper.Health := 1.0;
                Eeper.Size := (3, 3);
                Eeper.Attack_Cooldown := GUARD_ATTACK_COOLDOWN;
                exit;
            end if;
        end loop;
    end;

    type Level_Cell is (
      Level_None,
      Level_Gnome,
      Level_Urmom,
      Level_Guard,
      Level_Floor,
      Level_Wall,
      Level_Door,
      Level_Checkpoint,
      Level_Bomb_Gen,
      Level_Barricade,
      Level_Key,
      Level_Player,
      Level_New_Game,
      Level_Final);
    Level_Cell_Color: constant array (Level_Cell) of Color := [
      Level_None       => Get_Color(16#00000000#),
      Level_Gnome      => Get_Color(16#FF9600FF#),
      Level_Urmom      => Get_Color(16#96FF00FF#),
      Level_Guard      => Get_Color(16#00FF00FF#),
      Level_Floor      => Get_Color(16#FFFFFFFF#),
      Level_Wall       => Get_Color(16#000000FF#),
      Level_Door       => Get_Color(16#00FFFFFF#),
      Level_Checkpoint => Get_Color(16#FF00FFFF#),
      Level_Bomb_Gen   => Get_Color(16#FF0000FF#),
      Level_Barricade  => Get_Color(16#FF0096FF#),
      Level_Key        => Get_Color(16#FFFF00FF#),
      Level_Player     => Get_Color(16#0000FFFF#),
      Level_New_Game   => Get_Color(16#FFAAFFFF#),
      Level_Final      => Get_Color(16#265FDAFF#)];

    function Cell_By_Color(Col: Color; Out_Cel: out Level_Cell) return Boolean is
    begin
        for Cel in Level_Cell loop
            if Level_Cell_Color(Cel) = Col then
                Out_Cel := Cel;
                return True;
            end if;
        end loop;
        return False;
    end;

    procedure Load_Game_From_Image(File_Name: in String; Game: in out Game_State; Update_Player: Boolean) is
        type Color_Array is array (Natural range <>) of aliased Raylib.Color;
        package Color_Pointer is new Interfaces.C.Pointers(
          Index => Natural,
          Element => Raylib.Color,
          Element_Array => Color_Array,
          Default_Terminator => (others => 0));
        function To_Color_Pointer is new Ada.Unchecked_Conversion (Raylib.Addr, Color_Pointer.Pointer);
        use Color_Pointer;

        Img: constant Image := Raylib.Load_Image(To_C(File_Name));
        Pixels: constant Color_Pointer.Pointer := To_Color_Pointer(Img.Data);
    begin
        if Game.Map /= null then
            Delete_Map(Game.Map);
        end if;
        Game.Map := new Map(1..Integer(Img.Height), 1..Integer(Img.Width));

        for Eeper of Game.Eepers loop
            Eeper.Dead := True;
            if Eeper.Path /= null then
                Delete_Path_Map(Eeper.Path);
            end if;
            Eeper.Path := new Path_Map(1..Integer(Img.Height), 1..Integer(Img.Width));
            for Y in Eeper.Path'Range(1) loop
                for X in Eeper.Path'Range(2) loop
                    Eeper.Path(Y, X) := -1;
                end loop;
            end loop;
        end loop;

        Game.Items.Clear;
        for Bomb of Game.Bombs loop
            Bomb.Countdown := 0;
        end loop;

        for Row in Game.Map'Range(1) loop
            for Column in Game.Map'Range(2) loop
                declare
                    Index: constant Ptrdiff_T := Ptrdiff_T((Row - 1)*Integer(Img.Width) + (Column - 1));
                    Pixel: constant Color_Pointer.Pointer := Pixels + Index;
                    Cel: Level_Cell;
                begin
                    if Cell_By_Color(Pixel.all, Cel) then
                        case Cel is
                            when Level_None =>
                                Game.Map(Row, Column) := None;
                            when Level_Gnome =>
                                Spawn_Gnome(Game, (Column, Row));
                                Game.Map(Row, Column) := Floor;
                            when Level_Urmom =>
                                Spawn_Urmom(Game, (Column, Row));
                                Game.Map(Row, Column) := Floor;
                            when Level_Guard =>
                                Spawn_Guard(Game, (Column, Row));
                                Game.Map(Row, Column) := Floor;
                            when Level_Final =>
                                Spawn_Final(Game, (Column, Row));
                                Game.Map(Row, Column) := Floor;
                            when Level_Floor => Game.Map(Row, Column) := Floor;
                            when Level_Wall => Game.Map(Row, Column) := Wall;
                            when Level_Door => Game.Map(Row, Column) := Door;
                            when Level_Checkpoint =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), (Kind => Checkpoint));
                            when Level_Bomb_Gen =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), (Kind => Bomb_Gen, Cooldown => 0));
                            when Level_Barricade =>
                                Game.Map(Row, Column) := Barricade;
                            when Level_Key =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), (Kind => Key));
                            when Level_Player =>
                                Game.Map(Row, Column) := Floor;
                                if Update_Player then
                                    Game.Player.Position := (Column, Row);
                                    Game.Player.Prev_Position := (Column, Row);
                                end if;
                            when Level_New_Game =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), (Kind => New_Game));
                        end case;
                    else
                        Game.Map(Row, Column) := None;
                    end if;
                end;
            end loop;
        end loop;
    end;

    procedure Draw_Bomb(Position: IVector2; C: Color) is
    begin
        Draw_Circle_V(To_Vector2(Position)*Cell_Size + Cell_Size*0.5, Cell_Size.X*0.5, C);
    end;

    procedure Draw_Key(Position: IVector2) is
    begin
        Draw_Circle_V(To_Vector2(Position)*Cell_Size + Cell_Size*0.5, Cell_Size.X*0.25, Palette_RGB(COLOR_DOOR_KEY));
    end;

    procedure Draw_Number(Start, Size: Vector2; N: Integer; C: Color) is
        Label: constant Char_Array := To_C(Trim(Integer'Image(N), Ada.Strings.Left));
        Label_Height: constant Integer := 32;
        Label_Width: constant Integer := Integer(Measure_Text(Label, Int(Label_Height)));
        Text_Size: constant Vector2 := To_Vector2((Label_Width, Label_Height));
        Position: constant Vector2 := Start + Size*0.5 - Text_Size*0.5;
    begin
        Draw_Text(Label, Int(Position.X), Int(Position.Y), Int(Label_Height), C);
    end;

    procedure Draw_Number(Cell_Position: IVector2; N: Integer; C: Color) is
    begin
        Draw_Number(To_Vector2(Cell_Position)*Cell_Size, Cell_Size, N, C);
    end;

    procedure Game_Cells(Game: in Game_State) is
    begin
        for Row in Game.Map'Range(1) loop
            for Column in Game.Map'Range(2) loop
                declare
                    Position: constant Vector2 := To_Vector2((Column, Row))*Cell_Size;
                begin
                    Draw_Rectangle_V(position, cell_size, Cell_Colors(Game.Map(Row, Column)));
                end;
            end loop;
        end loop;
    end;

    procedure Game_Items(Game: in Game_State) is
        use Hashed_Map_Items;
    begin
        for C in Game.Items.Iterate loop
            case Element(C).Kind is
                when New_Game =>
                    declare
                        New_Game_Size: constant Vector2 := Cell_Size*0.8;
                    begin
                        Draw_Rectangle_V(To_Vector2(Key(C))*Cell_Size + Cell_Size*0.5 - New_Game_Size*0.5, New_Game_Size, Palette_RGB(COLOR_NEW_GAME));
                    end;
                when Key => Draw_Key(Key(C));
                when Checkpoint =>
                    declare
                        Checkpoint_Item_Size: constant Vector2 := Cell_Size*0.5;
                    begin
                        Draw_Rectangle_V(To_Vector2(Key(C))*Cell_Size + Cell_Size*0.5 - Checkpoint_Item_Size*0.5, Checkpoint_Item_Size, Palette_RGB(COLOR_CHECKPOINT));
                    end;
                when Bomb_Gen =>
                    if Element(C).Cooldown > 0 then
                        Draw_Bomb(Key(C), Color_Brightness(Palette_RGB(COLOR_BOMB), -0.5));
                        Draw_Number(Key(C), Element(C).Cooldown, Palette_RGB(COLOR_LABEL));
                    else
                        Draw_Bomb(Key(C), Palette_RGB(COLOR_BOMB));
                    end if;
            end case;
        end loop;
    end;

    procedure Open_Adjacent_Doors(Game: in out Game_State; Start: IVector2) is
        Q: Queue.Vector;
    begin
        if not Within_Map(Game, Start) or else Game.Map(Start.Y, Start.X) /= Door then
            return;
        end if;

        Game.Map(Start.Y, Start.X) := Floor;
        Q.Append(Start);

        while not Q.Is_Empty loop
            declare
                Position: constant IVector2 := Q(0);
            begin
                Q.Delete_First;

                for Dir in Direction loop
                    declare
                        New_Position: constant IVector2 := Position + Direction_Vector(Dir);
                    begin
                        if Within_Map(Game, New_Position) and then Game.Map(New_Position.Y, New_Position.X) = Door then
                            Game.Map(New_Position.Y, New_Position.X) := Floor;
                            Q.Append(New_Position);
                        end if;
                    end;
                end loop;
            end;
        end loop;
    end;

    procedure Game_Player_Turn(Game: in out Game_State; Dir: Direction) is
        New_Position: constant IVector2 := Game.Player.Position + Direction_Vector(Dir);
    begin
        Game.Player.Prev_Position := Game.Player.Position;
        Game.Turn_Animation := 1.0;

        if not Within_Map(Game, New_Position) then
            return;
        end if;

        case Game.Map(New_Position.Y, New_Position.X) is
           when Floor =>
               Game.Player.Position := New_Position;
               declare
                   use Hashed_Map_Items;
                   C: Cursor := Game.Items.Find(New_Position);
               begin
                   if Has_Element(C) then
                       case Element(C).Kind is
                           when New_Game =>
                               Load_Game_From_Image("map.png", Game, Update_Player => True);
                           when Key =>
                               Game.Player.Keys := Game.Player.Keys + 1;
                               Game.Items.Delete(C);
                           when Bomb_Gen => if
                               Game.Player.Bombs < Game.Player.Bomb_Slots
                               and then Element(C).Cooldown <= 0
                           then
                               Game.Player.Bombs := Game.Player.Bombs + 1;
                               Game.Items.Replace_Element(C, (Kind => Bomb_Gen, Cooldown => BOMB_GENERATOR_COOLDOWN));
                           end if;
                           when Checkpoint =>
                               Game.Items.Delete(C);
                               Game_Save_Checkpoint(Game);
                       end case;
                   end if;
               end;
           when Door =>
               if Game.Player.Keys > 0 then
                   Game.Player.Keys := Game.Player.Keys - 1;
                   Open_Adjacent_Doors(Game, New_Position);
                   Game.Player.Position := New_Position;
               end if;
           when others => null;
        end case;
    end;

    procedure Explode(Game: in out Game_State; Position: in IVector2) is
        procedure Explode_Line(Dir: Direction) is
            New_Position: IVector2 := Position;
        begin
            Line: for I in 1..EXPLOSION_LENGTH loop
                if not Within_Map(Game, New_Position) then
                    return;
                end if;

                case Game.Map(New_Position.Y, New_Position.X) is
                   when Floor | Explosion =>
                       Game.Map(New_Position.Y, New_Position.X) := Explosion;

                       if New_Position = Game.Player.Position then
                           Game.Player.Dead := True;
                           return;
                       end if;

                       for Eeper of Game.Eepers loop
                           if not Eeper.Dead and then Inside_Of_Rect(Eeper.Position, Eeper.Size, New_Position) then
                               case Eeper.Kind is
                                   when Eeper_Final => null;
                                   when Eeper_Gnome =>
                                       Game.Items.Insert(Eeper.Position, (Kind => Key));
                                       Eeper.Dead := True;
                                   when Eeper_Guard =>
                                       Eeper.Health := Eeper.Health - BOSS_EXPLOSION_DAMAGE;
                                       if Eeper.Health <= 0.0 then
                                           Eeper.Dead := True;
                                       end if;
                                   when Eeper_Urmom =>
                                       declare
                                           Position: constant IVector2 := Eeper.Position;
                                       begin
                                           Eeper.Dead := True;
                                           Spawn_Guard(Game, Position + (0, 0));
                                           Spawn_Guard(Game, Position + (4, 0));
                                           Spawn_Guard(Game, Position + (0, 4));
                                           Spawn_Guard(Game, Position + (4, 4));
                                       end;
                               end case;
                               return;
                           end if;
                       end loop;

                       New_Position := New_Position + Direction_Vector(Dir);
                   when Barricade =>
                       Game.Map(New_Position.Y, New_Position.X) := Explosion;
                       return;
                   when others =>
                       return;
                end case;
            end loop Line;
        end;
    begin
        for Dir in Direction loop
            Explode_Line(Dir);
        end loop;
    end;

    Keys: constant array (Direction) of int := [
        Left  => KEY_A,
        Right => KEY_D,
        Up    => KEY_W,
        Down  => KEY_S
    ];

    function Screen_Size return Vector2 is
    begin
        return To_Vector2((Integer(Get_Screen_Width), Integer(Get_Screen_Height)));
    end;

    procedure Game_Update_Camera(Game: in out Game_State) is
        Camera_Target: constant Vector2 :=
          Screen_Size*0.5 - To_Vector2(Game.Player.Position)*Cell_Size - Cell_Size*0.5;
    begin
        Game.Camera_Position := Game.Camera_Position + Game.Camera_Velocity*Get_Frame_Time;
        Game.Camera_Velocity := (Camera_Target - Game.Camera_Position)*2.0;
    end;

    function Game_Camera(Game: in Game_State) return Camera2D is
    begin
        return (
          offset => Game.Camera_Position,
          target => (x => 0.0, y => 0.0),
          rotation => 0.0,
          zoom => 1.0);
    end;

    function Interpolate_Positions(IPrev_Position, IPosition: IVector2; T: Float) return Vector2 is
        Prev_Position: constant Vector2 := To_Vector2(IPrev_Position)*Cell_Size;
        Curr_Position: constant Vector2 := To_Vector2(IPosition)*Cell_Size;
    begin
        return Vector2_Lerp(Prev_Position, Curr_Position, C_Float(1.0 - T*T));
    end;

    Space_Down: Boolean := False;
    Dir_Pressed: array (Direction) of Boolean := [others => False];

    procedure Swallow_Player_Input is
    begin
        Space_Down := False;
        Dir_Pressed := [others => False];
    end;

    procedure Game_Bombs_Turn(Game: in out Game_State) is
    begin
        for Bomb of Game.Bombs loop
            if Bomb.Countdown > 0 then
                Bomb.Countdown := Bomb.Countdown - 1;
                if Bomb.Countdown <= 0 then
                    Explode(Game, Bomb.Position);
                end if;
            end if;
        end loop;
    end;

    procedure Game_Explosions_Turn(Game: in out Game_State) is
    begin
        for Y in Game.Map'Range(1) loop
            for X in Game.Map'Range(2) loop
                if Game.Map(Y, X) = Explosion then
                    Game.Map(Y, X) := Floor;
                end if;
            end loop;
        end loop;
    end;

    procedure Game_Eepers_Turn(Game: in out Game_State) is
    begin
        for Me in Eeper_Index loop
            if not Game.Eepers(Me).Dead then
                Game.Eepers(Me).Prev_Position := Game.Eepers(Me).Position;
                Game.Eepers(Me).Prev_Eyes := Game.Eepers(Me).Eyes;
                case Game.Eepers(Me).Kind is
                    when Eeper_Final => null;
                    when Eeper_Guard | Eeper_Urmom =>
                        Recompute_Path_For_Eeper(Game, Me, GUARD_STEPS_LIMIT, GUARD_STEP_LENGTH_LIMIT);
                        if Game.Eepers(Me).Path(Game.Eepers(Me).Position.Y, Game.Eepers(Me).Position.X) >= 0 then
                            if Game.Eepers(Me).Attack_Cooldown <= 0 then
                                declare
                                    Current : constant Integer := Game.Eepers(Me).Path(Game.Eepers(Me).Position.Y, Game.Eepers(Me).Position.X);
                                begin
                                Search: for Dir in Direction loop
                                        declare
                                            Position: IVector2 := Game.Eepers(Me).Position;
                                        begin
                                            while Eeper_Can_Stand_Here(Game, Position, Me) loop
                                                Position := Position + Direction_Vector(Dir);
                                                if Within_Map(Game, Position) and then Game.Eepers(Me).Path(Position.Y, Position.X) = Current - 1 then
                                                    Game.Eepers(Me).Position := Position;
                                                    exit Search;
                                                end if;
                                            end loop;
                                        end;
                                    end loop Search;
                                end;
                                Game.Eepers(Me).Attack_Cooldown := GUARD_ATTACK_COOLDOWN;
                            else
                                Game.Eepers(Me).Attack_Cooldown := Game.Eepers(Me).Attack_Cooldown - 1;
                            end if;

                            if Game.Eepers(Me).Path(Game.Eepers(Me).Position.Y, Game.Eepers(Me).Position.X) = 1 then
                                Game.Eepers(Me).Eyes := Eyes_Angry;
                            else
                                Game.Eepers(Me).Eyes := Eyes_Open;
                            end if;
                        else
                            Game.Eepers(Me).Eyes := Eyes_Closed;
                            Game.Eepers(Me).Attack_Cooldown := GUARD_ATTACK_COOLDOWN + 1;
                        end if;

                        if Inside_Of_Rect(Game.Eepers(Me).Position, Game.Eepers(Me).Size, Game.Player.Position) then
                            Game.Player.Dead := True;
                        end if;
                        if Game.Eepers(Me).Health < 1.0 then
                            Game.Eepers(Me).Health := Game.Eepers(Me).Health + GUARD_TURN_REGENERATION;
                        end if;
                    when Eeper_Gnome =>
                        Recompute_Path_For_Eeper(Game, Me, 10, 1, Stop_At_Me => False);
                        declare
                            Position: constant IVector2 := Game.Eepers(Me).Position;
                        begin
                            if Game.Eepers(Me).Path(Position.Y, Position.X) >= 0 then
                                declare
                                    Available_Positions: array (0..Direction_Vector'Length-1) of IVector2;
                                    Count: Integer := 0;
                                begin
                                    for Dir in Direction loop
                                        declare
                                            New_Position: constant IVector2 := Position + Direction_Vector(Dir);
                                        begin
                                            if Within_Map(Game, New_Position)
                                              and then Game.Map(New_Position.Y, New_Position.X) = Floor
                                              and then Game.Eepers(Me).Path(New_Position.Y, New_Position.X) > Game.Eepers(Me).Path(Position.Y, Position.X)
                                            then
                                                Available_Positions(Count) := New_Position;
                                                Count := Count + 1;
                                            end if;
                                        end;
                                    end loop;

                                    if Count > 0 then
                                        Game.Eepers(Me).Position := Available_Positions(Random_Integer.Random(Gen) mod Count);
                                    end if;
                                end;
                                Game.Eepers(Me).Eyes := Eyes_Open;
                            else
                                Game.Eepers(Me).Eyes := Eyes_Closed;
                            end if;
                        end;
                end case;
            end if;
        end loop;
    end;

    procedure Game_Items_Turn(Game: in out Game_State) is
        use Hashed_Map_Items;
    begin
        for C in Game.Items.Iterate loop
            if Element(C).Kind = Bomb_Gen then
                if Element(C).Cooldown > 0 then
                    Game.Items.Replace_Element(C, (Kind => Bomb_Gen, Cooldown => Element(C).Cooldown - 1));
                end if;
            end if;
        end loop;
    end;

    function Screen_Player_Position(Game: in Game_State) return Vector2 is
    begin
        if Game.Turn_Animation > 0.0 then
            return Interpolate_Positions(Game.Player.Prev_Position, Game.Player.Position, Game.Turn_Animation);
        else
            return To_Vector2(Game.Player.Position)*Cell_Size;
        end if;
    end;

    procedure Game_Player(Game: in out Game_State) is
    begin
        if Game.Player.Dead then
            if Space_Down then
                Game_Restore_Checkpoint(Game);
                Game.Player.Dead := False;
            end if;

            return;
        end if;

        Draw_Rectangle_V(Screen_Player_Position(Game), Cell_Size, Palette_RGB(COLOR_PLAYER));

        if Game.Turn_Animation > 0.0 then
            return;
        end if;

        if Space_Down and then Game.Player.Bombs > 0 then
            for Dir in Direction loop
                declare
                    Position: constant IVector2 := Game.Player.Position + Direction_Vector(Dir);
                begin
                    if Within_Map(Game, Position) and then Game.Map(Position.Y, Position.X) = Floor then
                        Draw_Bomb(Position, Palette_RGB(COLOR_BOMB));
                        if Dir_Pressed(Dir) then
                            for Bomb of Game.Bombs loop
                                if Bomb.Countdown <= 0 then
                                    Bomb.Countdown := 3;
                                    Bomb.Position := Position;
                                    exit;
                                end if;
                            end loop;
                            Game.Player.Bombs := Game.Player.Bombs - 1;
                        end if;
                    end if;
                end;
            end loop;
        else
            for Dir in Direction loop
                if Dir_Pressed(Dir) then
                    declare
                        Start_Of_Turn: constant Double := Get_Time;
                    begin
                        Game_Explosions_Turn(Game);
                        Game_Player_Turn(Game, Dir);
                        Game_Bombs_Turn(Game);
                        Game_Items_Turn(Game);
                        Game_Eepers_Turn(Game);
                        Game.Duration_Of_Last_Turn := Get_Time - Start_Of_Turn;
                    end;
                end if;
            end loop;
        end if;
    end;

    procedure Game_Bombs(Game: Game_State) is
    begin
        for Bomb of Game.Bombs loop
            if Bomb.Countdown > 0 then
                Draw_Bomb(Bomb.Position, Palette_RGB(COLOR_BOMB));
                Draw_Number(Bomb.Position, Bomb.Countdown, Palette_RGB(COLOR_LABEL));
            end if;
        end loop;
    end;

    procedure Game_Hud(Game: in Game_State) is
    begin
        for Index in 1..Game.Player.Keys loop
            declare
                Position: constant Vector2 := (100.0 + C_float(Index - 1)*Cell_Size.X, 100.0);
            begin
                Draw_Circle_V(Position, Cell_Size.X*0.25, Palette_RGB(COLOR_DOOR_KEY));
            end;
        end loop;

        for Index in 1..Game.Player.Bombs loop
            declare
                Position: constant Vector2 := (100.0 + C_float(Index - 1)*Cell_Size.X, 200.0);
            begin
                Draw_Circle_V(Position, Cell_Size.X*0.5, Palette_RGB(COLOR_BOMB));
            end;
        end loop;

        if Game.Player.Dead then
            declare
                Label: constant Char_Array := To_C("Ded");
                Label_Height: constant Integer := 48;
                Label_Width: constant Integer := Integer(Measure_Text(Label, Int(Label_Height)));
                Text_Size: constant Vector2 := To_Vector2((Label_Width, Label_Height));
                Position: constant Vector2 := Screen_Size*0.5 - Text_Size*0.5;
            begin
                Draw_Text(Label, Int(Position.X), Int(Position.Y), Int(Label_Height), Palette_RGB(COLOR_LABEL));
            end;
        end if;
    end;

    procedure Health_Bar(Boundary_Start, Boundary_Size: Vector2; Health: C_Float) is
        Health_Padding: constant C_Float := 10.0;
        Health_Height: constant C_Float := 10.0;
        Health_Width: constant C_Float := Boundary_Size.X*Health;
    begin
        Draw_Rectangle_V(
          Boundary_Start - (0.0, Health_Padding + Health_Height),
          (Health_Width, Health_Height),
          Palette_RGB(COLOR_HEALTHBAR));
    end;

    procedure Draw_Eyes(Start, Size: Vector2; Angle: Float; Prev_Kind, Kind: Eyes_Kind; T: Float) is
        Dir: constant Vector2 := Vector2_Rotate((1.0, 0.0), C_Float(Angle));
        Eyes_Ratio: constant Vector2 := (13.0/64.0, 23.0/64.0);
        Eyes_Size: constant Vector2 := Eyes_Ratio*Size;
        Center: constant Vector2 := Start + Size*0.5;
        Position: constant Vector2 := Center + Dir*Eyes_Size.X*0.6;
        Positions: constant array (Eye) of Vector2 := [
            Left_Eye => Position - Eyes_Size*(0.5, 0.0) - Eyes_Size*(1.0, 0.5),
            Right_Eye => Position + Eyes_Size*(0.5, 0.0) - Eyes_Size*(0.0, 0.5)
        ];
        Mesh: Eye_Mesh;
    begin
        for Eye_Index in Eye loop
            for Vertex_Index in Eye_Mesh'Range loop
                Mesh(Vertex_Index) := Positions(Eye_Index) + Eyes_Size*Vector2_Lerp(Eyes_Meshes(Prev_Kind)(Eye_Index)(Vertex_Index), Eyes_Meshes(Kind)(Eye_Index)(Vertex_Index), C_Float(1.0 - T*T));
            end loop;
            Draw_Triangle_Strip(Mesh, Palette_RGB(COLOR_EYES));
        end loop;
    end;

    procedure Draw_Cooldown_Timer_Bubble(Start, Size: Vector2; Cooldown: Integer; Background: Palette) is
        Text_Color: constant Color := (A => 255, others => 0);
        Bubble_Radius: constant C_Float := 30.0;
        Bubble_Center: constant Vector2 := Start + Size*(0.5, 0.0) - (0.0, Bubble_Radius*2.0);
    begin
        Draw_Circle_V(Bubble_Center, Bubble_Radius, Palette_RGB(Background));
        Draw_Number(Bubble_Center - (Bubble_Radius, Bubble_Radius), (Bubble_Radius, Bubble_Radius)*2.0, Cooldown, Text_Color);
    end;

    procedure Game_Eepers(Game: in out Game_State) is
    begin
        for Eeper of Game.Eepers loop
            declare
                Position: constant Vector2 :=
                  (if Game.Turn_Animation > 0.0
                   then Interpolate_Positions(Eeper.Prev_Position, Eeper.Position, Game.Turn_Animation)
                   else To_Vector2(Eeper.Position)*Cell_Size);
                Size: constant Vector2 := To_Vector2(Eeper.Size)*Cell_Size;
            begin
                if not Eeper.Dead then
                    case Eeper.Kind is
                        when Eeper_Final =>
                            Draw_Rectangle_V(Position, Size, Palette_RGB(Eeper.Background));
                            Draw_Eyes(Position, Size, -Float(Vector2_Line_Angle(Position + Size*0.5, Screen_Player_Position(Game) + Cell_Size*0.5)), Eyes_Closed, Eyes_Closed, 1.0);
                        when Eeper_Guard | Eeper_Urmom =>
                            Draw_Rectangle_V(Position, Size, Palette_RGB(Eeper.Background));
                            Health_Bar(Position, Size, C_Float(Eeper.Health));
                            if Eeper.Path(Eeper.Position.Y, Eeper.Position.X) = 1 then
                                Draw_Cooldown_Timer_Bubble(Position, Size, Eeper.Attack_Cooldown, Eeper.Background);
                            elsif Eeper.Path(Eeper.Position.Y, Eeper.Position.X) >= 0 then
                                Draw_Cooldown_Timer_Bubble(Position, Size, Eeper.Attack_Cooldown, Eeper.Background);
                            end if;
                            Draw_Eyes(Position, Size, -Float(Vector2_Line_Angle(Position + Size*0.5, Screen_Player_Position(Game) + Cell_Size*0.5)), Eeper.Prev_Eyes, Eeper.Eyes, Game.Turn_Animation);
                        when Eeper_Gnome =>
                            declare
                                GNOME_RATIO: constant C_Float := 0.7;
                                GNOME_SIZE: constant Vector2 := Cell_Size*GNOME_RATIO;
                                GNOME_START: constant Vector2 := Position + Cell_Size*0.5 - GNOME_SIZE*0.5;
                            begin
                                Draw_Rectangle_V(GNOME_START, GNOME_SIZE, Palette_RGB(Eeper.Background));
                                Draw_Eyes(GNOME_START, GNOME_SIZE, -Float(Vector2_Line_Angle(GNOME_START + GNOME_SIZE*0.5, Screen_Player_Position(Game) + Cell_Size*0.5)), Eeper.Prev_Eyes, Eeper.Eyes, Game.Turn_Animation);
                            end;
                    end case;
                end if;
            end;
        end loop;
    end;

    Game: Game_State;
    Title: constant Char_Array := To_C("Eepers");

    Palette_Editor: Boolean := False;
    Palette_Editor_Choice: Palette := Palette'First;
    Palette_Editor_Selected: Boolean := False;
    Palette_Editor_Component: HSV_Comp := Hue;

begin
    Random_Integer.Reset(Gen);
    Load_Colors("colors.txt");
    Load_Game_From_Image("map.png", Game, True);
    Game_Save_Checkpoint(Game);
    Put_Line("Keys: " & Integer'Image(Game.Player.Keys));
    Set_Config_Flags(FLAG_WINDOW_RESIZABLE);
    Init_Window(800, 600, Title);
    Set_Target_FPS(60);
    Set_Exit_Key(KEY_NULL);
    while not Window_Should_Close loop
        Begin_Drawing;
            Clear_Background(Palette_RGB(COLOR_BACKGROUND));

            Space_Down := Boolean(Is_Key_Down(KEY_SPACE));
            for Dir in Direction loop
                Dir_Pressed(Dir) := Boolean(Is_Key_Pressed(Keys(Dir)));
            end loop;

            if DEVELOPMENT then
                if Is_Key_Pressed(KEY_R) then
                    Load_Game_From_Image("map.png", Game, False);
                end if;

                if Is_Key_Pressed(KEY_O) then
                    Palette_Editor := not Palette_Editor;
                    if not Palette_Editor then
                        Save_Colors("colors.txt");
                    end if;
                end if;

                if Palette_Editor then
                    if Palette_Editor_Selected then
                        if Is_Key_Pressed(KEY_ESCAPE) then
                            Palette_Editor_Selected := False;
                        end if;

                        if Is_Key_Pressed(Keys(Left)) then
                            if Palette_Editor_Component /= HSV_Comp'First then
                                Palette_Editor_Component := HSV_Comp'Pred(Palette_Editor_Component);
                            end if;
                        end if;

                        if Is_Key_Pressed(Keys(Right)) then
                            if Palette_Editor_Component /= HSV_Comp'Last then
                                Palette_Editor_Component := HSV_Comp'Succ(Palette_Editor_Component);
                            end if;
                        end if;

                        if Is_Key_Down(Keys(Up)) then
                            Palette_HSV(Palette_Editor_Choice)(Palette_Editor_Component) := Palette_HSV(Palette_Editor_Choice)(Palette_Editor_Component) + 1;
                            Palette_RGB(Palette_Editor_Choice) := HSV_To_RGB(Palette_HSV(Palette_Editor_Choice));
                        end if;

                        if Is_Key_Down(Keys(Down)) then
                            Palette_HSV(Palette_Editor_Choice)(Palette_Editor_Component) := Palette_HSV(Palette_Editor_Choice)(Palette_Editor_Component) - 1;
                            Palette_RGB(Palette_Editor_Choice) := HSV_To_RGB(Palette_HSV(Palette_Editor_Choice));
                        end if;
                    else
                        if Is_Key_Pressed(Keys(Down)) then
                            if Palette_Editor_Choice /= Palette'Last then
                                Palette_Editor_Choice := Palette'Succ(Palette_Editor_Choice);
                            end if;
                        end if;

                        if Is_Key_Pressed(Keys(Up)) then
                            if Palette_Editor_Choice /= Palette'First then
                                Palette_Editor_Choice := Palette'Pred(Palette_Editor_Choice);
                            end if;
                        end if;

                        if Is_Key_Pressed(KEY_ESCAPE) then
                            Palette_Editor := False;
                        end if;

                        if Is_Key_Pressed(KEY_ENTER) then
                            Palette_Editor_Selected := True;
                        end if;
                    end if;

                    Swallow_Player_Input;
                end if;
            end if;

            if Game.Turn_Animation > 0.0 then
                Game.Turn_Animation := (Game.Turn_Animation*TURN_DURATION_SECS - Float(Get_Frame_Time))/TURN_DURATION_SECS;
            end if;

            Game_Update_Camera(Game);
            Begin_Mode2D(Game_Camera(Game));
                Game_Cells(Game);
                Game_Items(Game);
                Game_Player(Game);
                Game_Eepers(Game);
                Game_Bombs(Game);
                if DEVELOPMENT then
                    if Is_Key_Down(KEY_P) then
                        for Row in Game.Map'Range(1) loop
                            for Column in Game.Map'Range(2) loop
                                Draw_Number((Column, Row), Game.Eepers(1).Path(Row, Column), (A => 255, others => 0));
                            end loop;
                        end loop;
                    end if;
                end if;
            End_Mode2D;

            Game_Hud(Game);
            if DEVELOPMENT then
                Draw_FPS(10, 10);
                declare
                    S: String(1..20);
                begin
                    Double_IO.Put(S, Game.Duration_Of_Last_Turn, Exp => 0);
                    Draw_Text(To_C(S), 100, 10, 32, (others => 255));
                end;
            end if;

            if Palette_Editor then
                for C in Palette loop
                    declare
                        Label: constant Char_Array := To_C(C'Image);
                        Label_Height: constant Integer := 32;
                        Position: constant Vector2 := (200.0, 200.0 + C_Float(Palette'Pos(C))*C_Float(Label_Height));
                    begin
                        Draw_Text(Label, Int(Position.X), Int(Position.Y), Int(Label_Height),
                          (if not Palette_Editor_Selected and C = Palette_Editor_Choice
                           then (R => 255, A => 255, others => 0)
                           else (others => 255)));

                        for Comp in HSV_Comp loop
                            declare
                                Label: constant Char_Array := To_C(Comp'Image & ": " & Palette_HSV(C)(Comp)'Image);
                                Label_Height: constant Integer := 32;
                                Position: constant Vector2 := (
                                    X => 600.0 + 200.0*C_Float(HSV_Comp'Pos(Comp)),
                                    Y => 200.0 + C_Float(Palette'Pos(C))*C_Float(Label_Height)
                                );
                            begin
                                Draw_Text(Label, Int(Position.X), Int(Position.Y), Int(Label_Height),
                                  (if Palette_Editor_Selected and C = Palette_Editor_Choice and Comp = Palette_Editor_Component
                                   then (R => 255, A => 255, others => 0)
                                   else (others => 255)));
                            end;
                        end loop;
                    end;
                end loop;
            end if;
        End_Drawing;
    end loop;
    Close_Window;
end;

--  TODO: Rename some definitions within the code
--    - Eeper -> Eeper
--    - Urmom -> Mother
--    - New_Game -> Father
--  TODO: Smarter Path Finding
--    - Recompute Path Map on each boss move. Not the Player turn. Because each Eeper position change may affect the Path Map
--    - Move Eepers starting from the closest to the Player. You can find the distance in the current Path Map.
--  TODO: During Path Finding maybe pick the equal paths randomly.
--    to introduce a bit of RNG into this pretty deterministic game
--  TODO: Path finding considers explosion impenetrable @bug
--  TODO: Bug with pushing Eepers back on timer 0
--  TODO: Place bombs directly at the Player's position
--  TODO: Disallow placing bomb on the same position more than once
--    Especially important if we gonna allow placing bombs at the position of the Player
--  TODO: Do not stack up damage for Eepers per the tiles of their body.
--  TODO: Keys for the bomb gens of final boss
--  TODO: Second boss room is boring
--  TODO: Eyes of Father changing as the Player gets closer:
--    - Closed
--    - Open
--    - Happy (very important to indicate that he's not hostile)
--  TODO: Keep rendering Player on Turn_Animation even if Dead
--    Because otherwise it just awkwardly disappears before running into the thing that killed them.
--  TODO: Eye Angle Speed
--    For smoother transitions. Especially from Open to Closed if we decide
--    that the Closed eyes should always point down
--  TODO: Surprised Eeper face you run into it by yourself (something like this o_O)
--  TODO: Set working directory to where the exe is
--    So it will search the resources there.
--  TODO: Eyes for the Player.
--    The denote last direction of the step.
--  TODO: Touch father starts NG+
--  TODO: Eeper should attack on zero just like a bomb.
--  TODO: Desaturate the colors
--  TODO: Properly disablable DEV features
--  TODO: Default 16:9 resolution
--  TODO: Fullscreen mode
--  TODO: Compile MinGW build with Windows Subsystem (so it does not show cmd window)
--  TODO: Don't reset cooldown timer for bosses (might be duplicate)
--  TODO: Try MSAA (if too slow, don't)
--  TODO: Bake assets into executable
--  TODO: Rename executable to "eepers"
--  TODO: Icon on for Windows build
--  TODO: Checkpoints must refill the bombs
--  TODO: Closed eyes should always point down
--  TODO: Special Eeper Eyes on Damage
--  TODO: Show Eeper Cooldown timer outside of the screen somehow
--  TODO: Visual Clue that the Eeper is about to kill the Player when Completely outside of the Screen
--    - Cooldown ball is shaking
--  TODO: Cool animation for New Game
--  TODO: The role of Barriers is not explored enough
--  TODO: Tutorial sign that says "WASD" to move when you start the game for the first time
--  TODO: "Tutorial" does not "explain" how to place bomb @content
--  TODO: keep steping while you are holding a certain direction
--    Cause constantly tapping it feels like ass.
--  TODO: count the player's turns towards the final score of the game
--    We can even collect different stats, like bombs collected, bombs used,
--    times died etc.
--  TODO: Animate key when you pick it up
--    Smoothly move it into the HUD.
--  TODO: Different palettes depending on the area
--    Or maybe different palette for each NG+
--  TODO: Eeper slide attack animation is pretty boring @polish
--  TODO: Restart on any key press after ded
--  TODO: Sounds
--  TODO: Player Death animation
--    Particles
--  TODO: Eeper Death animation
--    Particles
--  TODO: Cool effects when you pick up items and checkpoints
--    Particles
--  TODO: Allow moving with arrows too
--  TODO: Camera shaking when big bosses (Guard and Urmom) make moves
--  TODO: Initial position of the camera in map.png
--    The Father's position.
--  TODO: Indicate how many bomb slots we have in HUD
--  TODO: Menu
--    Could be just a splash with the game name and logo.
--  TODO: WebAssembly build
--    https://blog.adacore.com/use-of-gnat-llvm-to-translate-ada-applications-to-webassembly
--  TODO: Placing a bomb is not a turn (should it be tho?)
--  TODO: Explosions should trigger other primed bombs?
--  TODO: Path finding in a separate thread
--  TODO: Player pushing bombs mechanic
--    May not be relevant after we do not make bombs barriers
--  TODO: Gnome should have triangular hats in the form of keys
--    And key must become triangles intead of circles.
--    I don't think it's relevant anymore within the current lore of the game.
--  TODO: When the player revives themselves they are being put into bomb selection mode if they hold the space key which is weird.
--    May not be relevant if we allow placing bombs directly on the player position,
--    because then the direction selection mechanism becomes obsolete.
--  TODO: Primed bombs should be barriers
--    - Be careful with the order of Path Finding Map Recomputation
--    and the Player Bomb Placement. Map must be recomputed only after
--    the bombs are placed for the turn. This is related to making placement
--    of the bombs a legit turn.
--    - That enables escaping first boss btw.
--    - I think they should be not. Because on top of breaking bosses
--    they remove the possibility to spawn the bomb on the player.
--    Something that makes the controls actually simpler.
