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

procedure Game is
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
      COLOR_SHREK,
      COLOR_URMOM,
      COLOR_CHECKPOINT,
      COLOR_EXPLOSION,
      COLOR_HEALTHBAR);

    Palette_Names: constant array (Palette) of Unbounded_String := [
      COLOR_BACKGROUND => To_Unbounded_String("Background"),
      COLOR_FLOOR      => To_Unbounded_String("Floor"),
      COLOR_WALL       => To_Unbounded_String("Wall"),
      COLOR_Barricade  => To_Unbounded_String("Barricade"),
      COLOR_PLAYER     => To_Unbounded_String("Player"),
      COLOR_DOOR_KEY   => To_Unbounded_String("DoorKey"),
      COLOR_BOMB       => To_Unbounded_String("Bomb"),
      COLOR_LABEL      => To_Unbounded_String("Label"),
      COLOR_SHREK      => To_Unbounded_String("Shrek"),
      COLOR_URMOM      => To_Unbounded_String("Urmom"),
      COLOR_CHECKPOINT => To_Unbounded_String("Checkpoint"),
      COLOR_EXPLOSION  => To_Unbounded_String("Explosion"),
      COLOR_HEALTHBAR  => To_Unbounded_String("Healthbar")
      ];

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
            Put(F, To_String(Palette_Names(C)));
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
                        if Key = Palette_Names(C) then
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
    SHREK_ATTACK_COOLDOWN   : constant Integer := 10;
    BOSS_EXPLOSION_DAMAGE  : constant Float := 0.45;
    SHREK_TURN_REGENERATION : constant Float := 0.01;
    BOMB_GENERATOR_COOLDOWN : constant Integer := 20;
    SHREK_STEPS_LIMIT       : constant Integer := 4;
    SHREK_STEP_LENGTH_LIMIT : constant Integer := 100;
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

    type Item_Kind is (Key, Bomb, Checkpoint);

    type Item(Kind: Item_Kind := Key) is record
        case Kind is
            when Bomb =>
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

    type Boss_Behavior is (Shrek, Urmom);

    type Boss_State is record
        Behavior: Boss_Behavior;
        Dead: Boolean := True;
        Prev_Position: IVector2;
        Position: IVector2;
        Background: Palette;
        Size: IVector2 := (3, 3);
        Health: Float := 1.0;
        Attack_Cooldown: Integer := SHREK_ATTACK_COOLDOWN;
        Path: Path_Map_Access;
    end record;

    type Bomb_State is record
        Position: IVector2;
        Countdown: Integer := 0;
    end record;

    type Bomb_State_Array is array (1..10) of Bomb_State;

    type Boss_Index is range 1..10;
    type Boss_Array is array (Boss_Index) of Boss_State;

    type Checkpoint_State is record
        Map: Map_Access := Null;
        Player_Position: IVector2;
        Player_Keys: Integer;
        Player_Bombs: Integer;
        Player_Bomb_Slots: Integer;
        Bosses: Boss_Array;
        Items: Hashed_Map_Items.Map;
    end record;

    type Game_State is record
        Map: Map_Access := Null;
        Player: Player_State;
        Bosses: Boss_Array;
        --  Shrek: Boss_State;
        --  Urmom: Shrek_State := (
        --    Size => (6, 6));

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

    function Boss_Can_Stand_Here(Game: Game_State; Start: IVector2; Me: Boss_Index) return Boolean is
        Size: constant IVector2 := Game.Bosses(Me).Size;
    begin
        for X in Start.X..Start.X+Size.X-1 loop
            for Y in Start.Y..Start.Y+Size.Y-1 loop
                if not Within_Map(Game, (X, Y)) then
                    return False;
                end if;
                if Game.Map(Y, X) /= Floor then
                    return False;
                end if;
                for Index in Boss_Index loop
                    if not Game.Bosses(Index).Dead and then Index /= Me then
                        declare
                            Boss : constant Boss_State := Game.Bosses(Index);
                        begin
                            if Inside_Of_Rect(Boss.Position, Boss.Size, (X, Y)) then
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

    procedure Recompute_Path_For_Boss
      (Game: in out Game_State;
       Me: Boss_Index;
       Steps_Limit: Integer;
       Step_Length_Limit: Integer)
    is
        Q: Queue.Vector;
    begin
        for Y in Game.Bosses(Me).Path'Range(1) loop
            for X in Game.Bosses(Me).Path'Range(2) loop
                Game.Bosses(Me).Path(Y, X) := -1;
            end loop;
        end loop;

        for Dy in 0..Game.Bosses(Me).Size.Y-1 loop
            for Dx in 0..Game.Bosses(Me).Size.X-1 loop
                declare
                    Position: constant IVector2 := Game.Player.Position - (Dx, Dy);
                begin
                    if Boss_Can_Stand_Here(Game, Position, Me) then
                        Game.Bosses(Me).Path(Position.Y, Position.X) := 0;
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

                if Position = Game.Bosses(Me).Position then
                    exit;
                end if;

                if Game.Bosses(Me).Path(Position.Y, Position.X) >= Steps_Limit then
                    exit;
                end if;

                for Dir in Direction loop
                    declare
                        New_Position: IVector2 := Position + Direction_Vector(Dir);
                    begin
                        for Limit in 1..Step_Length_Limit loop
                            if not Boss_Can_Stand_Here(Game, New_Position, Me) then
                                exit;
                            end if;
                            if Game.Bosses(Me).Path(New_Position.Y, New_Position.X) >= 0 then
                                exit;
                            end if;
                            Game.Bosses(Me).Path(New_Position.Y, New_Position.X) := Game.Bosses(Me).Path(Position.Y, Position.X) + 1;
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
        Game.Checkpoint.Bosses            := Game.Bosses;
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
        Game.Bosses            := Game.Checkpoint.Bosses;
        Game.Items             := Game.Checkpoint.Items;
    end;

    procedure Spawn_Shrek(Game: in out Game_State; Position: IVector2) is
    begin
        for Boss of Game.Bosses loop
            if Boss.Dead then
                Boss.Behavior := Shrek;
                Boss.Background := COLOR_SHREK;
                Boss.Dead := False;
                Boss.Position := Position;
                Boss.Prev_Position := Position;
                Boss.Health := 1.0;
                Boss.Size := (3, 3);
                Boss.Attack_Cooldown := SHREK_ATTACK_COOLDOWN;
                exit;
            end if;
        end loop;
    end;

    procedure Load_Game_From_File(File_Name: in String; Game: in out Game_State; Update_Player: Boolean) is
        package Rows is new
            Ada.Containers.Vectors(
                Index_Type => Natural,
                Element_Type => Unbounded_String);
        F: File_Type;
        Map_Rows: Rows.Vector;
        Width: Integer := 0;
        Height: Integer := 0;
    begin
        Open(F, In_File, File_Name);
        while not End_Of_File(F) loop
            declare
                Line: constant String := Get_Line(F);
            begin
                if Line'Length > Width then
                    Width := Line'Length;
                end if;
                Map_Rows.Append(To_Unbounded_String(Line));
                Height := Height + 1;
            end;
        end loop;
        Close(F);

        if Game.Map /= null then
            Delete_Map(Game.Map);
        end if;
        Game.Map := new Map(1..Height, 1..Width);

        for Boss of Game.Bosses loop
            if Boss.Path /= null then
                Delete_Path_Map(Boss.Path);
            end if;
            Boss.Path := new Path_Map(1..Height, 1..Width);
        end loop;

        Game.Items.Clear;
        for Bomb of Game.Bombs loop
            Bomb.Countdown := 0;
        end loop;

        for Row in Game.Map'Range(1) loop
            declare
                Map_Row: constant Unbounded_String := Map_Rows(Row - 1);
            begin
                Put_Line(To_String(Map_Rows(Row - 1)));
                for Column in Game.Map'Range(2) loop
                    if Column in 1..Length(Map_Row) then
                        case Element(Map_Row, Column) is
                            when 'M' =>
                                for Boss of Game.Bosses loop
                                    if Boss.Dead then
                                        Boss.Behavior := Urmom;
                                        Boss.Dead := False;
                                        Boss.Background := COLOR_URMOM;
                                        Boss.Position := (Column, Row);
                                        Boss.Prev_Position := (Column, Row);
                                        Boss.Health := 1.0;
                                        Boss.Size := (6, 6);
                                        exit;
                                    end if;
                                end loop;
                                Game.Map(Row, Column) := Floor;
                            when 'B' =>
                                Spawn_Shrek(Game, (Column, Row));
                                Game.Map(Row, Column) := Floor;
                            when '.' => Game.Map(Row, Column) := Floor;
                            when '#' => Game.Map(Row, Column) := Wall;
                            when '=' => Game.Map(Row, Column) := Door;
                            when '!' =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), (Kind => Checkpoint));
                            when '*' =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), (Kind => Bomb, Cooldown => 0));
                            when '&' =>
                                Game.Map(Row, Column) := Barricade;
                            when '%' =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), (Kind => Key));
                            when '@' =>
                                Game.Map(Row, Column) := Floor;
                                if Update_Player then
                                    Game.Player.Position := (Column, Row);
                                    Game.Player.Prev_Position := (Column, Row);
                                end if;
                            when others => Game.Map(Row, Column) := None;
                        end case;
                    else
                        Game.Map(Row, Column) := None;
                    end if;
                end loop;
            end;
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
                    if Is_Key_Down(KEY_P) then
                        Draw_Number((Column, Row), Game.Bosses(1).Path(Row, Column), (A => 255, others => 0));
                    end if;
                end;
            end loop;
        end loop;
    end;

    procedure Game_Items(Game: in Game_State) is
        use Hashed_Map_Items;
    begin
        for C in Game.Items.Iterate loop
            case Element(C).Kind is
                when Key => Draw_Key(Key(C));
                when Checkpoint =>
                    declare
                        Checkpoint_Item_Size: constant Vector2 := Cell_Size*0.5;
                    begin
                        Draw_Rectangle_V(To_Vector2(Key(C))*Cell_Size + Cell_Size*0.5 - Checkpoint_Item_Size*0.5, Checkpoint_Item_Size, Palette_RGB(COLOR_CHECKPOINT));
                    end;
                when Bomb =>
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

    procedure Player_Step(Game: in out Game_State; Dir: Direction) is
        New_Position: constant IVector2 := Game.Player.Position + Direction_Vector(Dir);
    begin
        Game.Player.Prev_Position := Game.Player.Position;
        Game.Turn_Animation := 1.0;

        if not Within_Map(Game, New_Position) then
            return;
        end if;

        case Game.Map(New_Position.Y, New_Position.X) is
           when Floor =>
               declare
                   use Hashed_Map_Items;
                   C: Cursor := Game.Items.Find(New_Position);
               begin
                   if Has_Element(C) then
                       case Element(C).Kind is
                          when Key =>
                              Game.Player.Keys := Game.Player.Keys + 1;
                              Game.Items.Delete(C);
                          when Bomb => if Game.Player.Bombs < Game.Player.Bomb_Slots and then Element(C).Cooldown <= 0 then
                              Game.Player.Bombs := Game.Player.Bombs + 1;
                              Game.Items.Replace_Element(C, (Kind => Bomb, Cooldown => BOMB_GENERATOR_COOLDOWN));
                          end if;
                          when Checkpoint =>
                              Game.Items.Delete(C);
                              Game_Save_Checkpoint(Game);
                       end case;
                   end if;
               end;
               Game.Player.Position := New_Position;
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

                       for Boss of Game.Bosses loop
                           if not Boss.Dead and then Inside_Of_Rect(Boss.Position, Boss.Size, New_Position) then
                               case Boss.Behavior is
                                   when Shrek =>
                                       Boss.Health := Boss.Health - BOSS_EXPLOSION_DAMAGE;
                                       if Boss.Health <= 0.0 then
                                           Boss.Dead := True;
                                       end if;
                                   when Urmom =>
                                       declare
                                           Position: constant IVector2 := Boss.Position;
                                       begin
                                           Boss.Dead := True;
                                           Spawn_Shrek(Game, Position + (0, 0));
                                           Spawn_Shrek(Game, Position + (3, 0));
                                           Spawn_Shrek(Game, Position + (0, 3));
                                           Spawn_Shrek(Game, Position + (3, 3));
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
        return Prev_Position + (Curr_Position - Prev_Position)*C_Float(1.0 - T*T);
    end;

    Space_Down: Boolean := False;
    Dir_Pressed: array (Direction) of Boolean := [others => False];

    procedure Swallow_Player_Input is
    begin
        Space_Down := False;
        Dir_Pressed := [others => False];
    end;

    procedure Game_Player(Game: in out Game_State) is
    begin
        if Game.Player.Dead then
            --  TODO: when the player revives themselves they are
            --  being put into bomb selection mode if they hold the
            --  space key which is weird
            if Space_Down then
                Game_Restore_Checkpoint(Game);
                Game.Player.Dead := False;
            end if;

            return;
        end if;

        if Game.Turn_Animation > 0.0 then
            Draw_Rectangle_V(Interpolate_Positions(Game.Player.Prev_Position, Game.Player.Position, Game.Turn_Animation), Cell_Size, Palette_RGB(COLOR_PLAYER));
            return;
        end if;

        Draw_Rectangle_V(To_Vector2(Game.Player.Position)*Cell_Size, Cell_Size, Palette_RGB(COLOR_PLAYER));
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
                        for Y in Game.Map'Range(1) loop
                            for X in Game.Map'Range(2) loop
                                if Game.Map(Y, X) = Explosion then
                                    Game.Map(Y, X) := Floor;
                                end if;
                            end loop;
                        end loop;

                        Player_Step(Game, Dir);

                        for Bomb of Game.Bombs loop
                            if Bomb.Countdown > 0 then
                                Bomb.Countdown := Bomb.Countdown - 1;
                                if Bomb.Countdown <= 0 then
                                    Explode(Game, Bomb.Position);
                                end if;
                            end if;
                        end loop;

                        declare
                            use Hashed_Map_Items;
                        begin
                            for C in Game.Items.Iterate loop
                                if Element(C).Kind = Bomb then
                                    if Element(C).Cooldown > 0 then
                                        Game.Items.Replace_Element(C, (Kind => Bomb, Cooldown => Element(C).Cooldown - 1));
                                    end if;
                                end if;
                            end loop;
                        end;

                        for Me in Boss_Index loop
                            if not Game.Bosses(Me).Dead then
                                Recompute_Path_For_Boss(Game, Me, SHREK_STEPS_LIMIT, SHREK_STEP_LENGTH_LIMIT);
                                Game.Bosses(Me).Prev_Position := Game.Bosses(Me).Position;
                                -- TODO: Boss should attack on zero just like a bomb.
                                if Game.Bosses(Me).Attack_Cooldown <= 0 then
                                    declare
                                        Current : constant Integer := Game.Bosses(Me).Path(Game.Bosses(Me).Position.Y, Game.Bosses(Me).Position.X);
                                    begin
                                        -- TODO: maybe pick the paths
                                        --  randomly to introduce a bit of
                                        --  RNG into this pretty
                                        --  deterministic game
                                        Search: for Dir in Direction loop
                                            declare
                                                Position: IVector2 := Game.Bosses(Me).Position;
                                            begin
                                                while Boss_Can_Stand_Here(Game, Position, Me) loop
                                                    Position := Position + Direction_Vector(Dir);
                                                    if Within_Map(Game, Position) and then Game.Bosses(Me).Path(Position.Y, Position.X) = Current - 1 then
                                                        Game.Bosses(Me).Position := Position;
                                                        exit Search;
                                                    end if;
                                                end loop;
                                            end;
                                        end loop Search;
                                    end;
                                    Game.Bosses(Me).Attack_Cooldown := SHREK_ATTACK_COOLDOWN;
                                else
                                    Game.Bosses(Me).Attack_Cooldown := Game.Bosses(Me).Attack_Cooldown - 1;
                                end if;
                                if Inside_Of_Rect(Game.Bosses(Me).Position, Game.Bosses(Me).Size, Game.Player.Position) then
                                    Game.Player.Dead := True;
                                end if;
                                if Game.Bosses(Me).Health < 1.0 then
                                   Game.Bosses(Me).Health := Game.Bosses(Me).Health + SHREK_TURN_REGENERATION;
                                end if;
                            end if;
                        end loop;

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
        Health_Padding: constant C_Float := 20.0;
        Health_Height: constant C_Float := 10.0;
        Health_Width: constant C_Float := Boundary_Size.X*Health;
    begin
        Draw_Rectangle_V(
          Boundary_Start - (0.0, Health_Padding + Health_Height),
          (Health_Width, Health_Height),
          Palette_RGB(COLOR_HEALTHBAR));
    end;

    procedure Game_Bosses(Game: in out Game_State) is
    begin
        for Boss of Game.Bosses loop
            declare
                Position: constant Vector2 :=
                  (if Game.Turn_Animation > 0.0
                   then Interpolate_Positions(Boss.Prev_Position, Boss.Position, Game.Turn_Animation)
                   else To_Vector2(Boss.Position)*Cell_Size);
                Size: constant Vector2 := To_Vector2(Boss.Size)*Cell_Size;
            begin
                if not Boss.Dead then
                    Draw_Rectangle_V(Position, Cell_Size*To_Vector2(Boss.Size), Palette_RGB(Boss.Background));
                    case Boss.Behavior is
                        when Shrek =>
                            Health_Bar(Position, Size, C_Float(Boss.Health));
                        when Urmom =>
                            null;
                    end case;
                    Draw_Number(Position, Size, Boss.Attack_Cooldown, (A => 255, others => 0));
                end if;
            end;
        end loop;
    end;

    Game: Game_State;
    Title: constant Char_Array := To_C("Hello, NSA");

    Palette_Editor: Boolean := False;
    Palette_Editor_Choice: Palette := Palette'First;
    Palette_Editor_Selected: Boolean := False;
    Palette_Editor_Component: HSV_Comp := Hue;
begin
    Load_Colors("colors.txt");
    Load_Game_From_File("map.txt", Game, True);
    Game_Save_Checkpoint(Game);
    Put_Line("Keys: " & Integer'Image(Game.Player.Keys));
    Set_Config_Flags(FLAG_WINDOW_RESIZABLE);
    Init_Window(800, 600, Title);
    Set_Target_FPS(60);
    Set_Exit_Key(KEY_NULL);
    while Window_Should_Close = 0 loop
        Begin_Drawing;
            Clear_Background(Palette_RGB(COLOR_BACKGROUND));

            Space_Down := Boolean(Is_Key_Down(KEY_SPACE));
            for Dir in Direction loop
                Dir_Pressed(Dir) := Boolean(Is_Key_Pressed(Keys(Dir)));
            end loop;

            if DEVELOPMENT then
                if Is_Key_Pressed(KEY_R) then
                    Load_Game_From_File("map.txt", Game, False);
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
                Game_Bosses(Game);
                Game_Bombs(Game);
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
                        Label: constant Char_Array := To_C(To_String(Palette_Names(C)));
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

--  TODO: Side-room after first boss with Gnomes that drop keys to unlock bombs for the Second Boss @content
--  TODO: Smarter Path Finding
--    - Recompute Path Map on each boss move. Not the Player turn. Because each Boss position change may affect the Path Map
--    - Move Bosses starting from the closest to the Player. You can find the distance in the current Path Map.
--  TODO: Deactivate attack cooldown when the Boss cannot reach the player
--    - Don't show it
--    - Don't update it
--    - When it becomes active again start from the max value
--  TODO: Third Boss
--  TODO: Visual Clue that the Boss is about to kill the Player (only one step in Path Map)
--  TODO: Show Boss Cooldown timer outside of the screen somehow
--  TODO: "tutorial" does not "explain" how to place bomb @content
--  TODO: keep steping while you are holding a certain direction
--    Cause constantly tapping it feels like ass.
--  TODO: count the player's turns towards the final score of the game
--    We can even collect different stats, like bombs collected, bombs used,
--    times died etc.
--  TODO: Player pushing bombs mechanic
--  TODO: animate key when you pick it up
--    Smoothly move it into the HUD.
--  TODO: Different palettes depending on the area
--    Or maybe different palette for each NG+
--  TODO: Path finding considers explosion impenetrable
--  TODO: Sounds
--  TODO: Player Death animation @polish
--  TODO: Boss Death animation @polish
--  TODO: Cool effects when you pick up items and checkpoints @polish
--  TODO: Initial position of the camera in map.txt
--  TODO: Indicate how many bomb slots we have in HUD
--  TODO: Windows Build
--    https://www.adacore.com/download/more
--  TODO: Menu
--    resolution, volume, etc.
--  TODO: Allow moving with arrows too
--  TODO: Primed bombs should be barriers
--    Be careful with the order of Path Finding Map Recomputation
--    and the Player Bomb Placement. Map must be recomputed only after
--    the bombs are placed for the turn. This is related to making placement
--    of the bombs a legit turn.
--  TODO: placing a bomb is not a turn (should it be tho?)
--  TODO: NG+ which rounds you back to the beginning
