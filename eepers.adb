with Ada.Text_IO;
with Text_IO; use Text_IO;
with Interfaces.C; use Interfaces.C;
with Raylib; use Raylib;
with Raymath; use Raymath;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
use Ada.Containers;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Interfaces.C.Pointers;
with Ada.Unchecked_Conversion;
with Ada.Numerics; use Ada.Numerics;

procedure Eepers is
    package Random_Integer is
        new Ada.Numerics.Discrete_Random(Result_Subtype => Integer);
    Gen: Random_Integer.Generator;

    type Footsteps_Range is mod 4;
    Footsteps_Sounds: array (Footsteps_Range) of Sound;
    Footsteps_Pitches: constant array (Footsteps_Range) of C_Float := (1.7, 1.6, 1.5, 1.4);
    package Random_Footsteps is
        new Ada.Numerics.Discrete_Random(Result_Subtype => Footsteps_Range);
    Footsteps_Gen: Random_Footsteps.Generator;
    Blast_Sound: Sound;
    Key_Pickup_Sound: Sound;
    Bomb_Pickup_Sound: Sound;
    Open_Door_Sound: Sound;
    Checkpoint_Sound: Sound;
    Plant_Bomb_Sound: Sound;
    Guard_Step_Sound: Sound;
    Popup_Show_Sound: Sound;
    Ambient_Music: Music;
    Tutorial_Font: Font;
    Death_Font: Font;

    Tutorial_Font_Size: constant Int := 42;
    Death_Font_Size: constant Int := 68;

    DEVELOPMENT : constant Boolean := False;

    type Palette is (
      COLOR_BACKGROUND,
      COLOR_FLOOR,
      COLOR_WALL,
      COLOR_BARRICADE,
      COLOR_PLAYER,
      COLOR_DOORKEY,
      COLOR_BOMB,
      COLOR_LABEL,
      COLOR_GUARD,
      COLOR_MOTHER,
      COLOR_CHECKPOINT,
      COLOR_EXPLOSION,
      COLOR_HEALTHBAR,
      COLOR_EYES,
      COLOR_FATHER);

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

    procedure Increment(X: in out Integer) is
    begin
        X := X + 1;
    end;

    Palette_RGB: array (Palette) of Color := (others => (A => 255, others => 0));
    Palette_HSV: array (Palette) of HSV := (others => (others => 0));

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

    BASE_TURN_DURATION_SECS      : constant Float := 0.125;
    TURN_DURATION_SECS           : Float := BASE_TURN_DURATION_SECS;
    GUARD_ATTACK_COOLDOWN        : constant Integer := 10;
    EEPER_EXPLOSION_DAMAGE       : constant Float := 0.45;
    GUARD_TURN_REGENERATION      : constant Float := 0.01;
    BOMB_GENERATOR_COOLDOWN      : constant Integer := 10;
    GUARD_STEPS_LIMIT            : constant Integer := 10;
    GUARD_STEP_LENGTH_LIMIT      : constant Integer := 100;
    EXPLOSION_LENGTH             : constant Integer := 10;
    EYES_ANGULAR_VELOCITY        : constant Float := 10.0;
    TUTORIAL_MOVE_WAIT_TIME_SECS : constant C_Float := 5.0;
    TUTORIAL_BOMB_WAIT_TIME_SECS : constant C_Float := 4.0;
    TUTORIAL_SPRINT_WAIT_TIME_SECS : constant C_Float := 15.0;
    POPUP_ANIMATION_DURATION : constant C_Float := 0.1;
    RESTART_TIMEOUT_SECS : constant Double := 2.0;

    type IVector2 is record
        X, Y: Integer;
    end record;

	function "="(A, B: IVector2) return Boolean is
	begin
	    return A.X = B.X and then A.Y = B.Y;
	end;

    type Cell is (Cell_None, Cell_Floor, Cell_Wall, Cell_Barricade, Cell_Door, Cell_Explosion);
    Cell_Size : constant Vector2 := (x => 50.0, y => 50.0);

    function Cell_Colors(C: Cell) return Color is
    begin
        case C is
            when Cell_None      => return Palette_RGB(COLOR_BACKGROUND);
            when Cell_Floor     => return Palette_RGB(COLOR_FLOOR);
            when Cell_Wall      => return Palette_RGB(COLOR_WALL);
            when Cell_Barricade => return Palette_RGB(COLOR_BARRICADE);
            when Cell_Door      => return Palette_RGB(COLOR_DOORKEY);
            when Cell_Explosion => return Palette_RGB(COLOR_EXPLOSION);
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

    function "+"(A, B: IVector2) return IVector2 is
    begin
        return (A.X + B.X, A.Y + B.Y);
    end;

    function "-"(A, B: IVector2) return IVector2 is
    begin
        return (A.X - B.X, A.Y - B.Y);
    end;

    function "*"(A: IVector2; S: Integer) return IVector2 is
    begin
        return (A.X*S, A.Y*S);
    end;

    type Item_Kind is (
      Item_None,
      Item_Key,
      Item_Bomb_Refill,
      Item_Checkpoint,
      Item_Bomb_Slot);
    type Item is record
        Kind: Item_Kind := Item_None;
        Position: IVector2;
        Cooldown: Integer;
    end record;
    type Item_Index is range 1..100;
    type Item_Array is array (Item_Index) of Item;

    function To_Vector2(iv: IVector2) return Vector2 is
    begin
        return (X => C_float(iv.X), Y => C_float(iv.Y));
    end;

    type Eyes_Kind is (Eyes_Open, Eyes_Closed, Eyes_Angry, Eyes_Cringe, Eyes_Surprised);
    type Eye_Mesh is new Vector2_Array(1..4);
    type Eye is (Left_Eye, Right_Eye);
    type Eyes_Mesh is array (Eye) of Eye_Mesh;
    Eyes_Meshes: constant array (Eyes_Kind) of Eyes_Mesh := (
        Eyes_Open => (
            -- 1-3
            -- |/|
            -- 2-4
            Left_Eye => ( (0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0) ),
            -- 3-4
            -- |\|
            -- 1-2
            Right_Eye => ( (0.0, 1.0), (1.0, 1.0), (0.0, 0.0), (1.0, 0.0) )
        ),
        Eyes_Closed => (
            Left_Eye => ( (0.0, 0.8), (0.0, 1.0), (1.0, 0.8), (1.0, 1.0) ),
            Right_Eye => ( (0.0, 1.0), (1.0, 1.0), (0.0, 0.8), (1.0, 0.8) )
        ),
        Eyes_Angry => (
            Left_Eye => ( (0.0, 0.0), (0.0, 1.0), (1.0, 0.3), (1.0, 1.0) ),
            Right_Eye => ( (0.0, 1.0), (1.0, 1.0), (0.0, 0.3), (1.0, 0.0) )
        ),
        Eyes_Cringe => (
            Left_Eye => ( (0.0, 0.5), (0.25, 0.75), (1.3, 0.75), (0.0, 1.0) ),
            Right_Eye => ( (1.0, 1.0), (0.75, 0.75), (-0.3, 0.75), (1.0, 0.5) )
        ),
        Eyes_Surprised => (
            Left_Eye => ( (0.0, 0.3), (0.0, 1.0), (1.0, 0.3), (1.0, 1.0) ),
            Right_Eye => ( (0.0, 1.0), (1.0, 1.0), (0.0, 0.0), (1.0, 0.0) )
        )
    );

    type Player_State is record
        Prev_Position: IVector2;
        Position: IVector2;
        Prev_Eyes: Eyes_Kind;
        Eyes: Eyes_Kind;
        Eyes_Angle: Float;
        Eyes_Target: IVector2;
        Keys: Integer := 0;
        Bombs: Integer := 0;
        Bomb_Slots: Integer := 1;
        Dead: Boolean := False;
        Death_Time: Double;
    end record;

    type Eeper_Kind is (Eeper_Guard, Eeper_Mother, Eeper_Gnome, Eeper_Father);

    type Eeper_State is record
        Kind: Eeper_Kind;
        Dead: Boolean := True;
        Position, Prev_Position: IVector2;
        Eyes_Angle: Float;
        Eyes_Target: IVector2;
        Prev_Eyes: Eyes_Kind;
        Eyes: Eyes_Kind := Eyes_Closed;
        Size: IVector2;
        Path: Path_Map_Access;
        Damaged: Boolean;

        Background: Palette;
        Health: Float := 1.0;
        Attack_Cooldown: Integer := GUARD_ATTACK_COOLDOWN;
    end record;

    type Bomb_State is record
        Position: IVector2;
        Countdown: Integer := 0;
    end record;

    type Bomb_State_Array is array (1..10) of Bomb_State;

    type Eeper_Index is range 1..30;
    type Eeper_Array is array (Eeper_Index) of Eeper_State;

    type Checkpoint_State is record
        Map: Map_Access := Null;
        Player_Position: IVector2;
        Player_Keys: Integer;
        Player_Bombs: Integer;
        Player_Bomb_Slots: Integer;
        Eepers: Eeper_Array;
        Items: Item_Array;
        Bombs: Bomb_State_Array;
    end record;

    type Direction is (Left, Right, Up, Down);

    Direction_Vector: constant array (Direction) of IVector2 := (
      Left  => (X => -1, Y => 0),
      Right => (X => 1, Y => 0),
      Up    => (X => 0, Y => -1),
      Down  => (X => 0, Y => 1));

    type Popup_State is record
        Label: Char_Array(1..50);
        Visible: Boolean := False;
        Animation: C_Float := 0.0;
    end record;

    procedure Show_Popup(Popup: in out Popup_State; Text: String) is
        Ignore: Size_t;
    begin
        if not Popup.Visible then
            Play_Sound(Popup_Show_Sound);
        end if;
        Popup.Visible := True;
        To_C(Text, Popup.Label, Ignore);
    end;

    procedure Hide_Popup(Popup: in out Popup_State) is
    begin
        Popup.Visible := False;
    end;

    procedure Draw_Popup(Popup: in out Popup_State; Start, Size: Vector2) is
    begin
        if Popup.Visible then
            if Popup.Animation < 1.0 then
                Popup.Animation := (Popup.Animation*POPUP_ANIMATION_DURATION + Get_Frame_Time)/POPUP_ANIMATION_DURATION;
            end if;
        else
            if Popup.Animation > 0.0 then
                Popup.Animation := (Popup.Animation*POPUP_ANIMATION_DURATION - Get_Frame_Time)/POPUP_ANIMATION_DURATION;
            end if;
        end if;

        if Popup.Animation > 0.0 then
            declare
                Font_Size: constant C_Float := C_Float(Tutorial_Font_Size)*Popup.Animation;
                Popup_Bottom_Margin: constant C_Float := Font_Size*0.05;
                Label_Size: constant Vector2 := Measure_Text_Ex(Tutorial_Font, Popup.Label, Font_Size, 0.0);
                Label_Position: constant Vector2 := Start + Size*(0.5, 0.0) - Label_Size*(0.5, 1.0) - (0.0, Popup_Bottom_Margin);
            begin
                Draw_Text_Ex(Tutorial_Font, Popup.Label, Label_Position + (-2.0, 2.0), Font_Size, 0.0, Palette_RGB(COLOR_WALL));
                Draw_Text_Ex(Tutorial_Font, Popup.Label, Label_Position, Font_Size, 0.0, Palette_RGB(COLOR_PLAYER));
            end;
        end if;
    end;

    type Tutorial_Phase is (Tutorial_Move, Tutorial_Place_Bombs, Tutorial_Waiting_For_Sprint, Tutorial_Sprint, Tutorial_Done);
    type Tutorial_State is record
        Phase: Tutorial_Phase := Tutorial_Move;
        Waiting: C_Float := 0.0;

        Prev_Step_Timestamp: Double := 0.0;
        Hurry_Count: Integer := 0;

        Popup: Popup_State;

        Knows_How_To_Move: Boolean := False;
        Knows_How_To_Place_Bombs: Boolean := False;
        Knows_How_To_Sprint: Boolean := False;
    end record;

    type Game_State is record
        Map: Map_Access := Null;
        Player: Player_State;
        Eepers: Eeper_Array;

        Turn_Animation: Float := 0.0;

        Items: Item_Array;
        Bombs: Bomb_State_Array;
        Camera: Camera2D := (
          offset => (x => 0.0, y => 0.0),
          target => (x => 0.0, y => 0.0),
          rotation => 0.0,
          zoom => 1.0);

        Tutorial: Tutorial_State;
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
                -- NOTE: it's fine to step into the explosions, because they don't live long enough.
                -- They disappear on the next turn.
                if Game.Map(Y, X) /= Cell_Floor and Game.Map(Y, X) /= Cell_Explosion then
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
        Eeper: Eeper_State renames Game.Eepers(Me);
    begin
        for Y in Eeper.Path'Range(1) loop
            for X in Eeper.Path'Range(2) loop
                Eeper.Path(Y, X) := -1;
            end loop;
        end loop;

        for Dy in 0..Eeper.Size.Y-1 loop
            for Dx in 0..Eeper.Size.X-1 loop
                declare
                    Position: constant IVector2 := Game.Player.Position - (Dx, Dy);
                begin
                    if Eeper_Can_Stand_Here(Game, Position, Me) then
                        Eeper.Path(Position.Y, Position.X) := 0;
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

                if Stop_At_Me and then Position = Eeper.Position then
                    exit;
                end if;

                if Eeper.Path(Position.Y, Position.X) >= Steps_Limit then
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
                            if Eeper.Path(New_Position.Y, New_Position.X) >= 0 then
                                exit;
                            end if;
                            Eeper.Path(New_Position.Y, New_Position.X) := Eeper.Path(Position.Y, Position.X) + 1;
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
        Game.Checkpoint.Bombs             := Game.Bombs;
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
        Game.Bombs             := Game.Checkpoint.Bombs;
    end;

    function Allocate_Eeper(Game: in out Game_State) return Eeper_Index is
        Too_Many_Entities: exception;
    begin
        for Eeper in Game.Eepers'Range loop
            if Game.Eepers(Eeper).Dead then
                Game.Eepers(Eeper).Dead := False;
                return Eeper;
            end if;
        end loop;
        raise Too_Many_Entities;
    end;

    procedure Allocate_Item(Game: in out Game_State; Position: IVector2; Kind: Item_Kind) is
        Too_Many_Items: exception;
    begin
        for Item of Game.Items loop
            if Item.Kind = Item_None then
                Item.Kind := Kind;
                Item.Position := Position;
                Item.Cooldown := 0;
                return;
            end if;
        end loop;
        raise Too_Many_Items;
    end;

    procedure Spawn_Gnome(Game: in out Game_State; Position: IVector2) is
        Gnome: Eeper_State renames Game.Eepers(Allocate_Eeper(Game));
        Size: constant IVector2 := (1, 1);
    begin
        Gnome.Kind := Eeper_Gnome;
        Gnome.Prev_Eyes := Eyes_Closed;
        Gnome.Eyes := Eyes_Closed;
        Gnome.Eyes_Target := Position + (Size.X/2, Size.Y);
        Gnome.Background := COLOR_DOORKEY;
        Gnome.Position := Position;
        Gnome.Prev_Position := Position;
        Gnome.Size := Size;
    end;

    procedure Spawn_Father(Game: in out Game_State; Position: IVector2) is
        Father: Eeper_State renames Game.Eepers(Allocate_Eeper(Game));
        Size: constant IVector2 := (7, 7);
    begin
        Father.Kind := Eeper_Father;
        Father.Prev_Eyes := Eyes_Closed;
        Father.Eyes := Eyes_Closed;
        Father.Eyes_Angle := Pi*0.5;
        Father.Eyes_Target := Position + (Size.X/2, Size.Y);
        Father.Background := COLOR_FATHER;
        Father.Position := Position;
        Father.Prev_Position := Position;
        Father.Size := Size;
    end;

    procedure Spawn_Mother(Game: in out Game_State; Position: IVector2) is
        Mother: Eeper_State renames Game.Eepers(Allocate_Eeper(Game));
        Size: constant IVector2 := (7, 7);
    begin
        Mother.Kind := Eeper_Mother;
        Mother.Prev_Eyes := Eyes_Closed;
        Mother.Eyes := Eyes_Closed;
        Mother.Eyes_Target := Position + (Size.X/2, Size.Y);
        Mother.Background := COLOR_MOTHER;
        Mother.Position := Position;
        Mother.Prev_Position := Position;
        Mother.Health := 1.0;
        Mother.Size := Size;
    end;

    procedure Spawn_Guard(Game: in out Game_State; Position: IVector2) is
        Guard: Eeper_State renames Game.Eepers(Allocate_Eeper(Game));
        Size: constant IVector2 := (3, 3);
    begin
        Guard.Kind := Eeper_Guard;
        Guard.Prev_Eyes := Eyes_Closed;
        Guard.Eyes := Eyes_Closed;
        Guard.Eyes_Target := Position + (Size.X/2, Size.Y);
        Guard.Background := COLOR_GUARD;
        Guard.Position := Position;
        Guard.Prev_Position := Position;
        Guard.Health := 1.0;
        Guard.Size := Size;
        Guard.Attack_Cooldown := GUARD_ATTACK_COOLDOWN;
    end;

    type Level_Cell is (
      Level_None,
      Level_Gnome,
      Level_Mother,
      Level_Guard,
      Level_Floor,
      Level_Wall,
      Level_Door,
      Level_Checkpoint,
      Level_Bomb_Refill,
      Level_Barricade,
      Level_Key,
      Level_Player,
      Level_Father,
      Level_Bomb_Slot);
    Level_Cell_Color: constant array (Level_Cell) of Color := (
      Level_None        => Get_Color(16#00000000#),
      Level_Gnome       => Get_Color(16#FF9600FF#),
      Level_Mother      => Get_Color(16#96FF00FF#),
      Level_Guard       => Get_Color(16#00FF00FF#),
      Level_Floor       => Get_Color(16#FFFFFFFF#),
      Level_Wall        => Get_Color(16#000000FF#),
      Level_Door        => Get_Color(16#00FFFFFF#),
      Level_Checkpoint  => Get_Color(16#FF00FFFF#),
      Level_Bomb_Refill => Get_Color(16#FF0000FF#),
      Level_Barricade   => Get_Color(16#FF0096FF#),
      Level_Key         => Get_Color(16#FFFF00FF#),
      Level_Player      => Get_Color(16#0000FFFF#),
      Level_Father      => Get_Color(16#265FDAFF#),
      Level_Bomb_Slot   => Get_Color(16#BC5353FF#));

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

    function Screen_Size return Vector2 is
    begin
        return To_Vector2((Integer(Get_Screen_Width), Integer(Get_Screen_Height)));
    end;

    procedure Load_Game_From_Image(File_Name: in String; Game: in out Game_State; Update_Player: Boolean; Update_Camera: Boolean) is
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

        for Item of Game.Items loop
            Item.Kind := Item_None;
        end loop;
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
                                Game.Map(Row, Column) := Cell_None;
                            when Level_Gnome =>
                                Spawn_Gnome(Game, (Column, Row));
                                Game.Map(Row, Column) := Cell_Floor;
                            when Level_Mother =>
                                Spawn_Mother(Game, (Column, Row));
                                Game.Map(Row, Column) := Cell_Floor;
                            when Level_Guard =>
                                Spawn_Guard(Game, (Column, Row));
                                Game.Map(Row, Column) := Cell_Floor;
                            when Level_Father =>
                                if Update_Camera then
                                    Game.Camera.target := (To_Vector2((Column, Row)) + To_Vector2((7, 7))*0.5) * Cell_Size;
                                    Game.Camera.offset := Screen_Size*0.5 - Cell_Size*0.5;
                                end if;
                                Spawn_Father(Game, (Column, Row));
                                Game.Map(Row, Column) := Cell_Floor;
                            when Level_Floor => Game.Map(Row, Column) := Cell_Floor;
                            when Level_Wall => Game.Map(Row, Column) := Cell_Wall;
                            when Level_Door => Game.Map(Row, Column) := Cell_Door;
                            when Level_Checkpoint =>
                                Game.Map(Row, Column) := Cell_Floor;
                                Allocate_Item(Game, (Column, Row), Item_Checkpoint);
                            when Level_Bomb_Refill =>
                                Game.Map(Row, Column) := Cell_Floor;
                                Allocate_Item(Game, (Column, Row), Item_Bomb_Refill);
                            when Level_Bomb_Slot =>
                                Game.Map(Row, Column) := Cell_Floor;
                                Allocate_Item(Game, (Column, Row), Item_Bomb_Slot);
                            when Level_Barricade =>
                                Game.Map(Row, Column) := Cell_Barricade;
                            when Level_Key =>
                                Game.Map(Row, Column) := Cell_Floor;
                                Allocate_Item(Game, (Column, Row), Item_Key);
                            when Level_Player =>
                                Game.Map(Row, Column) := Cell_Floor;
                                if Update_Player then
                                    Game.Player.Position := (Column, Row);
                                    Game.Player.Prev_Position := (Column, Row);
                                    Game.Player.Bombs := 0;
                                    Game.Player.Keys := 0;
                                    -- TODO: should we save the state of the eyes in the checkpoint?
                                    --   Or maybe checkpoint should just save the entirety of the Player_State.
                                    --   'Cause that's what we do for Eepers anyway. It works for them.
                                    Game.Player.Prev_Eyes := Eyes_Closed;
                                    Game.Player.Eyes := Eyes_Open;
                                    Game.Player.Eyes_Angle := Pi*0.5;
                                    Game.Player.Eyes_Target := Game.Player.Position + Direction_Vector(Up);
                                end if;
                        end case;
                    else
                        Game.Map(Row, Column) := Cell_None;
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
        Draw_Circle_V(To_Vector2(Position)*Cell_Size + Cell_Size*0.5, Cell_Size.X*0.25, Palette_RGB(COLOR_DOORKEY));
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
    begin
        for Item of Game.Items loop
            case Item.Kind is
                when Item_None => null;
                when Item_Key => Draw_Key(Item.Position);
                when Item_Checkpoint =>
                    declare
                        Checkpoint_Item_Size: constant Vector2 := Cell_Size*0.5;
                    begin
                        Draw_Rectangle_V(To_Vector2(Item.Position)*Cell_Size + Cell_Size*0.5 - Checkpoint_Item_Size*0.5, Checkpoint_Item_Size, Palette_RGB(COLOR_CHECKPOINT));
                    end;
                when Item_Bomb_Refill =>
                    if Item.Cooldown > 0 then
                        Draw_Bomb(Item.Position, Color_Brightness(Palette_RGB(COLOR_BOMB), -0.5));
                        Draw_Number(Item.Position, Item.Cooldown, Palette_RGB(COLOR_LABEL));
                    else
                        Draw_Bomb(Item.Position, Palette_RGB(COLOR_BOMB));
                    end if;
                when Item_Bomb_Slot =>
                    Draw_Bomb(Item.Position, Palette_RGB(COLOR_DOORKEY));
            end case;
        end loop;
    end;

    procedure Flood_Fill(Game: in out Game_State; Start: IVector2; Fill: Cell) is
        Q: Queue.Vector;
        Background: Cell;
    begin
        if not Within_Map(Game, Start) then
            return;
        end if;

        Background := Game.Map(Start.Y, Start.X);
        Game.Map(Start.Y, Start.X) := Fill;
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
                        if Within_Map(Game, New_Position) and then Game.Map(New_Position.Y, New_Position.X) = Background then
                            Game.Map(New_Position.Y, New_Position.X) := Fill;
                            Q.Append(New_Position);
                        end if;
                    end;
                end loop;
            end;
        end loop;
    end;

    procedure Game_Player_Turn(Game: in out Game_State; Step_Direction: Direction) is
    begin
        Game.Player.Prev_Eyes := Game.Player.Eyes;
        Game.Player.Prev_Position := Game.Player.Position;

        declare
            New_Position: constant IVector2 := Game.Player.Position + Direction_Vector(Step_Direction);
        begin
            Game.Player.Eyes_Target := New_Position + Direction_Vector(Step_Direction);

            if not Within_Map(Game, New_Position) then
                return;
            end if;

            case Game.Map(New_Position.Y, New_Position.X) is
                when Cell_Floor =>
                    Game.Player.Position := New_Position;
                    for Item of Game.Items loop
                        if Item.Position = New_Position then
                            case Item.Kind is
                                when Item_None => null;
                                when Item_Key =>
                                    Game.Player.Keys := Game.Player.Keys + 1;
                                    Item.Kind := Item_None;
                                    Play_Sound(Key_Pickup_Sound);
                                when Item_Bomb_Refill => if
                                    Game.Player.Bombs < Game.Player.Bomb_Slots
                                    and then Item.Cooldown <= 0
                                then
                                    Game.Player.Bombs := Game.Player.Bombs + 1;
                                    Item.Cooldown := BOMB_GENERATOR_COOLDOWN;
                                    Play_Sound(Bomb_Pickup_Sound);
                                end if;
                                when Item_Bomb_Slot =>
                                    Item.Kind := Item_None;
                                    Increment(Game.Player.Bomb_Slots);
                                    Game.Player.Bombs := Game.Player.Bomb_Slots;
                                when Item_Checkpoint =>
                                    Item.Kind := Item_None;
                                    Game.Player.Bombs := Game.Player.Bomb_Slots;
                                    Game_Save_Checkpoint(Game);
                                    Play_Sound(Checkpoint_Sound);
                            end case;
                        end if;
                    end loop;
                when Cell_Door =>
                    if Game.Player.Keys > 0 then
                        Game.Player.Keys := Game.Player.Keys - 1;
                        Flood_Fill(Game, New_Position, Cell_Floor);
                        Game.Player.Position := New_Position;
                        Play_Sound(Open_Door_Sound);
                    end if;
                when others => null;
            end case;
            Play_Sound(Footsteps_Sounds(Random_Footsteps.Random(Footsteps_Gen)));
        end;
    end;

    procedure Kill_Player(Game: in out Game_State) is
    begin
        Game.Player.Dead := True;
        Game.Player.Death_Time := Get_Time;
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
                   when Cell_Floor | Cell_Explosion =>
                       Game.Map(New_Position.Y, New_Position.X) := Cell_Explosion;

                       if New_Position = Game.Player.Position then
                           Kill_Player(Game);
                       end if;

                       for Eeper of Game.Eepers loop
                           if not Eeper.Dead and then Inside_Of_Rect(Eeper.Position, Eeper.Size, New_Position) then
                               Eeper.Damaged := True;
                           end if;
                       end loop;

                       New_Position := New_Position + Direction_Vector(Dir);
                   when Cell_Barricade =>
                       Flood_Fill(Game, New_Position, Cell_Explosion);
                       Game.Map(New_Position.Y, New_Position.X) := Cell_Explosion;
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

    Keys: constant array (Direction) of int := (
        Left  => KEY_A,
        Right => KEY_D,
        Up    => KEY_W,
        Down  => KEY_S
    );

    procedure Game_Update_Camera(Game: in out Game_State) is
        Camera_Target: constant Vector2 := To_Vector2(Game.Player.Position)*Cell_Size;
        Camera_Offset: constant Vector2 := Screen_Size*0.5 - Cell_Size*0.5;
        Camera_Velocity: constant Vector2 := (Camera_Target - Game.Camera.target)*2.0;
    begin
        Game.Camera.offset := Camera_Offset;
        Game.Camera.target := Game.Camera.target + Camera_Velocity*Get_Frame_Time;
        --  TODO: animate zoom similarly to Game.Camera.target
        --    So it looks cool when you resize the game in the window mode.
        --  TODO: The tutorial signs look gross on bigger screens.
        --    We need to do something with the fonts
        Game.Camera.zoom := C_Float'Max(Screen_Size.x/1920.0, Screen_Size.y/1080.0);
    end;

    function Interpolate_Positions(IPrev_Position, IPosition: IVector2; T: Float) return Vector2 is
        Prev_Position: constant Vector2 := To_Vector2(IPrev_Position)*Cell_Size;
        Curr_Position: constant Vector2 := To_Vector2(IPosition)*Cell_Size;
    begin
        return Vector2_Lerp(Prev_Position, Curr_Position, C_Float(1.0 - T*T));
    end;

    type Command_Kind is (Command_Step, Command_Plant);
    type Command(Kind: Command_Kind := Command_Step) is record
        case Kind is
            when Command_Step => Dir: Direction;
            when Command_Plant => null;
        end case;
    end record;
    Command_Capacity: constant Natural := 3;
    type Command_Array is array (0..Command_Capacity-1) of Command;
    type Command_Queue_Record is record
        Items: Command_Array;
        Start: Natural := 0;
        Size: Natural := 0;
    end record;

    procedure Command_Enqueue(Q: in out Command_Queue_Record; C: Command) is
    begin
        Q.Items((Q.Start + Q.Size) mod Command_Capacity) := C;
        if Q.Size < Command_Capacity then
            Q.Size := Q.Size + 1;
        else
            Q.Start := (Q.Start + 1) mod Command_Capacity;
        end if;
    end;

    function Command_Dequeue(Q: in out Command_Queue_Record; C: out Command) return Boolean is
    begin
        if Q.Size = 0 then
            return False;
        end if;
        C := Q.Items(Q.Start);
        Q.Size := Q.Size - 1;
        Q.Start := (Q.Start + 1) mod Command_Capacity;
        return True;
    end;

    Command_Queue: Command_Queue_Record;
    Holding_Shift: Boolean := False;

    procedure Swallow_Player_Input is
    begin
        Command_Queue.Size := 0;
        Holding_Shift := False;
    end;

    procedure Game_Bombs_Turn(Game: in out Game_State) is
    begin
        for Eeper of Game.Eepers Loop
            Eeper.Damaged := False;
        end loop;
        for Bomb of Game.Bombs loop
            if Bomb.Countdown > 0 then
                Bomb.Countdown := Bomb.Countdown - 1;
                if Bomb.Countdown <= 0 then
                    Play_Sound(Blast_Sound);
                    Explode(Game, Bomb.Position);
                end if;
            end if;
        end loop;
        for Eeper of Game.Eepers loop
            if not Eeper.Dead and then Eeper.Damaged then
                case Eeper.Kind is
                    when Eeper_Father => null;
                    when Eeper_Mother =>
                        declare
                            Position: constant IVector2 := Eeper.Position;
                        begin
                            Eeper.Dead := True;
                            Spawn_Guard(Game, Position + (0, 0));
                            Spawn_Guard(Game, Position + (4, 0));
                            Spawn_Guard(Game, Position + (0, 4));
                            Spawn_Guard(Game, Position + (4, 4));
                        end;
                    when Eeper_Guard =>
                        Eeper.Eyes := Eyes_Cringe;
                        Eeper.Health := Eeper.Health - EEPER_EXPLOSION_DAMAGE;
                        if Eeper.Health <= 0.0 then
                            Eeper.Dead := True;
                        end if;
                    when Eeper_Gnome =>
                        Eeper.Dead := True;
                        Allocate_Item(Game, Eeper.Position, Item_Key);
                end case;
            end if;
        end loop;
    end;

    procedure Game_Explosions_Turn(Game: in out Game_State) is
    begin
        for Y in Game.Map'Range(1) loop
            for X in Game.Map'Range(2) loop
                if Game.Map(Y, X) = Cell_Explosion then
                    Game.Map(Y, X) := Cell_Floor;
                end if;
            end loop;
        end loop;
    end;

    function Look_At(Looker, Target: Vector2) return Float is
    begin
        return -Float(Vector2_Line_Angle(Looker, Target));
    end;

    procedure Game_Eepers_Turn(Game: in out Game_State) is
    begin
        for Me in Eeper_Index loop
            declare
                Eeper: Eeper_State renames Game.Eepers(Me);
            begin
                if not Eeper.Dead then
                    Eeper.Prev_Position := Eeper.Position;
                    Eeper.Prev_Eyes := Eeper.Eyes;
                    case Eeper.Kind is
                        when Eeper_Father =>
                            declare
                                Wake_Up_Radius: constant IVector2 := (3, 3);
                            begin
                                if Inside_Of_Rect(Eeper.Position, Eeper.Size, Game.Player.Position) then
                                    Load_Game_From_Image("assets/map.png", Game, Update_Player => True, Update_Camera => False);
                                    Game_Save_Checkpoint(Game);
                                    Play_Sound(Checkpoint_Sound);
                                elsif Inside_Of_Rect(Eeper.Position - Wake_Up_Radius, Eeper.Size + Wake_Up_Radius*2, Game.Player.Position) then
                                    Eeper.Eyes_Target := Game.Player.Position;
                                    Eeper.Eyes := Eyes_Open;
                                else
                                    Eeper.Eyes_Target := Eeper.Position + (Eeper.Size.X/2, Eeper.Size.Y);
                                    Eeper.Eyes := Eyes_Closed;
                                end if;
                            end;
                        when Eeper_Guard | Eeper_Mother =>
                            Recompute_Path_For_Eeper(Game, Me, GUARD_STEPS_LIMIT, GUARD_STEP_LENGTH_LIMIT);
                            if Eeper.Path(Eeper.Position.Y, Eeper.Position.X) = 0 then
                                Kill_Player(Game);
                                Eeper.Eyes := Eyes_Surprised;
                            elsif Eeper.Path(Eeper.Position.Y, Eeper.Position.X) > 0 then
                                if Eeper.Attack_Cooldown <= 0 then
                                    declare
                                        Current : constant Integer := Eeper.Path(Eeper.Position.Y, Eeper.Position.X);
                                        Available_Positions: array (0..Direction_Vector'Length-1) of IVector2;
                                        Count: Integer := 0;
                                    begin
                                        for Dir in Direction loop
                                            declare
                                                Position: IVector2 := Eeper.Position;
                                            begin
                                                while Eeper_Can_Stand_Here(Game, Position, Me) loop
                                                    Position := Position + Direction_Vector(Dir);
                                                    if Within_Map(Game, Position) and then Eeper.Path(Position.Y, Position.X) = Current - 1 then
                                                        Available_Positions(Count) := Position;
                                                        Count := Count + 1;
                                                        exit;
                                                    end if;
                                                end loop;
                                            end;
                                        end loop;
                                        if Count > 0 then
                                            Eeper.Position := Available_Positions(Random_Integer.Random(Gen) mod Count);
                                            Play_Sound(Guard_Step_Sound);
                                        end if;
                                    end;
                                    Eeper.Attack_Cooldown := GUARD_ATTACK_COOLDOWN;
                                else
                                    Eeper.Attack_Cooldown := Eeper.Attack_Cooldown - 1;
                                end if;

                                if Eeper.Path(Eeper.Position.Y, Eeper.Position.X) = 1 then
                                    Eeper.Eyes := Eyes_Angry;
                                else
                                    Eeper.Eyes := Eyes_Open;
                                end if;
                                Eeper.Eyes_Target := Game.Player.Position;

                                if Inside_Of_Rect(Eeper.Position, Eeper.Size, Game.Player.Position) then
                                    Kill_Player(Game);
                                end if;
                            else
                                Eeper.Eyes := Eyes_Closed;
                                Eeper.Eyes_Target := Eeper.Position + (Eeper.Size.X/2, Eeper.Size.Y);
                                Eeper.Attack_Cooldown := GUARD_ATTACK_COOLDOWN + 1;
                            end if;

                            if Eeper.Health < 1.0 then
                                Eeper.Health := Eeper.Health + GUARD_TURN_REGENERATION;
                            end if;
                        when Eeper_Gnome =>
                            Recompute_Path_For_Eeper(Game, Me, 9, 1, Stop_At_Me => False);
                            declare
                                Position: constant IVector2 := Eeper.Position;
                            begin
                                if Eeper.Path(Position.Y, Position.X) >= 0 then
                                    declare
                                        Available_Positions: array (0..Direction_Vector'Length-1) of IVector2;
                                        Count: Integer := 0;
                                    begin
                                        for Dir in Direction loop
                                            declare
                                                New_Position: constant IVector2 := Position + Direction_Vector(Dir);
                                            begin
                                                if Within_Map(Game, New_Position)
                                                    and then Game.Map(New_Position.Y, New_Position.X) = Cell_Floor
                                                    and then Eeper.Path(New_Position.Y, New_Position.X) > Eeper.Path(Position.Y, Position.X)
                                                then
                                                    Available_Positions(Count) := New_Position;
                                                    Count := Count + 1;
                                                end if;
                                            end;
                                        end loop;

                                        if Count > 0 then
                                            Eeper.Position := Available_Positions(Random_Integer.Random(Gen) mod Count);
                                        end if;
                                    end;
                                    Eeper.Eyes := Eyes_Open;
                                    Eeper.Eyes_Target := Game.Player.Position;
                                else
                                    Eeper.Eyes := Eyes_Closed;
                                    Eeper.Eyes_Target := Eeper.Position + (Eeper.Size.X/2, Eeper.Size.Y);
                                end if;
                            end;
                    end case;
                end if;
            end;
        end loop;
    end;

    procedure Game_Items_Turn(Game: in out Game_State) is
    begin
        for Item of Game.Items loop
            if Item.Kind = Item_Bomb_Refill then
                if Item.Cooldown > 0 then
                    Item.Cooldown := Item.Cooldown - 1;
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

    function Clamp(X, Lo, Hi: Float) return Float is
    begin
        if X < Lo then
            return Lo;
        elsif X > Hi then
            return Hi;
        else
            return X;
        end if;
    end;

    function Repeat(T, Length: Float) return Float is
        function Floorf(A: C_Float) return C_Float
            with
                Import => True,
                Convention => C,
                External_Name => "floorf";
    begin
        return Clamp(T - Float(Floorf(C_Float(T/Length)))*Length, 0.0, Length);
    end;

    function Delta_Angle(A, B: Float) return Float is
        Dlt: Float := Repeat(B - A, 2.0*Pi);
    begin
        if Dlt > Pi then
            Dlt := Dlt - 2.0*Pi;
        end if;
        return Dlt;
    end;

    procedure Draw_Eyes(Start, Size: Vector2; Angle: Float; Prev_Kind, Kind: Eyes_Kind; T: Float) is
        Dir: constant Vector2 := Vector2_Rotate((1.0, 0.0), C_Float(Angle));
        Eyes_Ratio: constant Vector2 := (13.0/64.0, 23.0/64.0);
        Eyes_Size: constant Vector2 := Eyes_Ratio*Size;
        Center: constant Vector2 := Start + Size*0.5;
        Position: constant Vector2 := Center + Dir*Eyes_Size.X*0.6;
        Positions: constant array (Eye) of Vector2 := (
            Left_Eye => Position - Eyes_Size*(0.5, 0.0) - Eyes_Size*(1.0, 0.5),
            Right_Eye => Position + Eyes_Size*(0.5, 0.0) - Eyes_Size*(0.0, 0.5)
        );
        Mesh: Eye_Mesh;
    begin
        for Eye_Index in Eye loop
            for Vertex_Index in Eye_Mesh'Range loop
                Mesh(Vertex_Index) := Positions(Eye_Index) + Eyes_Size*Vector2_Lerp(Eyes_Meshes(Prev_Kind)(Eye_Index)(Vertex_Index), Eyes_Meshes(Kind)(Eye_Index)(Vertex_Index), C_Float(1.0 - T*T));
            end loop;
            Draw_Triangle_Strip(Mesh, Palette_RGB(COLOR_EYES));
        end loop;
    end;

    procedure Game_Tutorial(Game: in out Game_State) is
    begin
        case Game.Tutorial.Phase is
            when Tutorial_Move =>
                if Game.Tutorial.Knows_How_To_Move then
                    Game.Tutorial.Phase := Tutorial_Place_Bombs;
                    Game.Tutorial.Waiting := 0.0;
                    Hide_Popup(Game.Tutorial.Popup);
                elsif Game.Tutorial.Waiting < TUTORIAL_MOVE_WAIT_TIME_SECS then
                    Game.Tutorial.Waiting := Game.Tutorial.Waiting + Get_Frame_Time;
                else
                    Show_Popup(Game.Tutorial.Popup, "WASD to Move");
                end if;
            when Tutorial_Place_Bombs =>
                if Game.Tutorial.Knows_How_To_Place_Bombs then
                    Game.Tutorial.Phase := Tutorial_Waiting_For_Sprint;
                    Hide_Popup(Game.Tutorial.Popup);
                elsif Game.Player.Bombs > 0 then
                    if Game.Tutorial.Waiting < TUTORIAL_BOMB_WAIT_TIME_SECS then
                        Game.Tutorial.Waiting := Game.Tutorial.Waiting + Get_Frame_Time;
                    else
                        Show_Popup(Game.Tutorial.Popup, "SPACE to Place Bombs");
                    end if;
                end if;
            when Tutorial_Waiting_For_Sprint =>
                if Game.Tutorial.Hurry_Count >= 10 then
                    Game.Tutorial.Phase := Tutorial_Sprint;
                    Game.Tutorial.Waiting := 0.0;
                end if;
            when Tutorial_Sprint =>
                if Game.Tutorial.Knows_How_To_Sprint then
                    Game.Tutorial.Phase := Tutorial_Done;
                    Hide_Popup(Game.Tutorial.Popup);
                else
                    if Game.Tutorial.Waiting < TUTORIAL_SPRINT_WAIT_TIME_SECS then
                        Show_Popup(Game.Tutorial.Popup, "Hold SHIFT to Sprint");
                        Game.Tutorial.Waiting := Game.Tutorial.Waiting + Get_Frame_Time;
                    else
                        Game.Tutorial.Phase := Tutorial_Done;
                        Hide_Popup(Game.Tutorial.Popup);
                    end if;
                end if;
            when Tutorial_Done => null;
        end case;
        Draw_Popup(Game.Tutorial.Popup, Screen_Player_Position(Game), Cell_Size);
    end;

    procedure Game_Player(Game: in out Game_State) is
        Eyes_Angular_Direction: constant Float := Delta_Angle(Game.Player.Eyes_Angle, Look_At(To_Vector2(Game.Player.Position)*Cell_Size + Cell_Size*0.5, To_Vector2(Game.Player.Eyes_Target)*Cell_Size + Cell_Size*0.5));
    begin
        Game.Player.Eyes_Angle := Game.Player.Eyes_Angle + Eyes_Angular_Direction*EYES_ANGULAR_VELOCITY*Float(Get_Frame_Time);
        if Game.Player.Dead then
            if Game.Turn_Animation >= 0.0 then
                Draw_Rectangle_V(Screen_Player_Position(Game), Cell_Size, Palette_RGB(COLOR_PLAYER));
                Draw_Eyes(Screen_Player_Position(Game), Cell_Size, Game.Player.Eyes_Angle, Game.Player.Prev_Eyes, Game.Player.Eyes, Game.Turn_Animation);
            end if;

            if (Get_Time - Game.Player.Death_Time) > RESTART_TIMEOUT_SECS then
                Game_Restore_Checkpoint(Game);
                Game.Player.Dead := False;
            end if;

            return;
        end if;

        Draw_Rectangle_V(Screen_Player_Position(Game), Cell_Size, Palette_RGB(COLOR_PLAYER));
        Draw_Eyes(Screen_Player_Position(Game), Cell_Size, Game.Player.Eyes_Angle, Game.Player.Prev_Eyes, Game.Player.Eyes, Game.Turn_Animation);

        if Game.Turn_Animation > 0.0 then
            return;
        end if;

        declare
            C: Command;
        begin
            if Command_Dequeue(Command_Queue, C) then
                case C.Kind is
                    when Command_Step =>
                        declare
                            Start_Of_Turn: constant Double := Get_Time;
                        begin
                            Game.Tutorial.Knows_How_To_Move := True;
                            if Holding_Shift then
                                Game.Tutorial.Knows_How_To_Sprint := True;
                            end if;

                            if Game.Tutorial.Phase = Tutorial_Waiting_For_Sprint then
                                declare
                                    Step_Timestamp: constant Double := Get_Time;
                                    Delta_Timestamp: constant Double := Step_Timestamp - Game.Tutorial.Prev_Step_Timestamp;
                                begin
                                    if Delta_Timestamp < 0.2 Then
                                        Game.Tutorial.Hurry_Count := Game.Tutorial.Hurry_Count + 1;
                                    elsif Game.Tutorial.Hurry_Count > 0 then
                                        Game.Tutorial.Hurry_Count := Game.Tutorial.Hurry_Count - 1;
                                    end if;
                                    Game.Tutorial.Prev_Step_Timestamp := Step_Timestamp;
                                end;
                            end if;

                            Game.Turn_Animation := 1.0;
                            Game_Explosions_Turn(Game);
                            Game_Items_Turn(Game);
                            Game_Player_Turn(Game, C.Dir);
                            Game_Bombs_Turn(Game);
                            Game_Eepers_Turn(Game);
                            Game.Duration_Of_Last_Turn := Get_Time - Start_Of_Turn;
                        end;
                    when Command_Plant =>
                        if  Game.Player.Bombs > 0 then
                            declare
                                Start_Of_Turn: constant Double := Get_Time;
                            begin
                                Game.Tutorial.Knows_How_To_Place_Bombs := True;

                                Game.Turn_Animation := 1.0;
                                Game_Explosions_Turn(Game);
                                Game_Items_Turn(Game);

                                --  Game_Player_Turn(Game, Action_Plant_Bomb, Left);
                                Game.Player.Prev_Eyes := Game.Player.Eyes;
                                Game.Player.Prev_Position := Game.Player.Position;

                                Game_Bombs_Turn(Game);
                                Game_Eepers_Turn(Game);

                                if Game.Player.Bombs > 0 then
                                    for Bomb of Game.Bombs loop
                                        if Bomb.Countdown <= 0 then
                                            Bomb.Countdown := 3;
                                            Bomb.Position := Game.Player.Position;
                                            exit;
                                        end if;
                                    end loop;
                                    Game.Player.Bombs := Game.Player.Bombs - 1;
                                    Play_Sound(Plant_Bomb_Sound);
                                end if;

                                Game.Duration_Of_Last_Turn := Get_Time - Start_Of_Turn;
                            end;
                        end if;
                end case;
            end if;
        end;
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
                Draw_Circle_V(Position, Cell_Size.X*0.25, Palette_RGB(COLOR_DOORKEY));
            end;
        end loop;

        for Index in 1..Game.Player.Bomb_Slots loop
            declare
                Padding: constant C_Float := Cell_Size.X*0.5;
                Position: constant Vector2 := (100.0 + C_float(Index - 1)*(Cell_Size.X + Padding), 200.0);
            begin
                if Index <= Game.Player.Bombs then
                    Draw_Circle_V(Position, Cell_Size.X*0.5, Palette_RGB(COLOR_BOMB));
                else
                    Draw_Circle_V(Position, Cell_Size.X*0.5, Color_Brightness(Palette_RGB(COLOR_BOMB), -0.5));
                end if;
            end;
        end loop;

        if Game.Player.Dead then
            declare
                Label: constant Char_Array := To_C("You Died!");
                Text_Size: constant Vector2 := Measure_Text_Ex(Death_Font, Label, C_Float(Death_Font_Size), 0.0);
                Position: constant Vector2 := Screen_Size*0.5 - Text_Size*0.5;
            begin
                Draw_Text_Ex(Death_Font, Label, Position + (-2.0, 2.0), C_Float(Death_Font_Size), 0.0, Palette_RGB(COLOR_WALL));
                Draw_Text_Ex(Death_Font, Label, Position, C_Float(Death_Font_Size), 0.0, Palette_RGB(COLOR_PLAYER));
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
                Eyes_Angular_Direction: constant Float := Delta_Angle(Eeper.Eyes_Angle, Look_At(Position + Size*0.5, To_Vector2(Eeper.Eyes_Target)*Cell_Size + Cell_Size*0.5));
            begin
                if not Eeper.Dead then
                    Eeper.Eyes_Angle := Eeper.Eyes_Angle + Eyes_Angular_Direction*EYES_ANGULAR_VELOCITY*Float(Get_Frame_Time);
                    case Eeper.Kind is
                        when Eeper_Father =>
                            Draw_Rectangle_V(Position, Size, Palette_RGB(Eeper.Background));
                            Draw_Eyes(Position, Size, Eeper.Eyes_Angle, Eeper.Prev_Eyes, Eeper.Eyes, Game.Turn_Animation);
                        when Eeper_Guard | Eeper_Mother =>
                            Draw_Rectangle_V(Position, Size, Palette_RGB(Eeper.Background));
                            Health_Bar(Position, Size, C_Float(Eeper.Health));
                            if Eeper.Path(Eeper.Position.Y, Eeper.Position.X) = 1 then
                                Draw_Cooldown_Timer_Bubble(Position, Size, Eeper.Attack_Cooldown, Eeper.Background);
                            elsif Eeper.Path(Eeper.Position.Y, Eeper.Position.X) >= 0 then
                                Draw_Cooldown_Timer_Bubble(Position, Size, Eeper.Attack_Cooldown, Eeper.Background);
                            end if;
                            Draw_Eyes(Position, Size, Eeper.Eyes_Angle, Eeper.Prev_Eyes, Eeper.Eyes, Game.Turn_Animation);
                        when Eeper_Gnome =>
                            declare
                                GNOME_RATIO: constant C_Float := 0.7;
                                GNOME_SIZE: constant Vector2 := Cell_Size*GNOME_RATIO;
                                GNOME_START: constant Vector2 := Position + Cell_Size*0.5 - GNOME_SIZE*0.5;
                            begin
                                Draw_Rectangle_V(GNOME_START, GNOME_SIZE, Palette_RGB(Eeper.Background));
                                Draw_Eyes(GNOME_START, GNOME_SIZE, Eeper.Eyes_Angle, Eeper.Prev_Eyes, Eeper.Eyes, Game.Turn_Animation);
                            end;
                    end case;
                end if;
            end;
        end loop;
    end;

    Game: Game_State;
    Title: constant Char_Array := To_C("Eepers (v1.4)");

    Palette_Editor: Boolean := False;
    Palette_Editor_Choice: Palette := Palette'First;
    Palette_Editor_Selected: Boolean := False;
    Palette_Editor_Component: HSV_Comp := Hue;
    Icon: Image;
begin
    if not Change_Directory(Get_Application_Directory) then
        Put_Line("WARNING: Could not change working directory to the application directory");
    end if;

    Icon := Load_Image("assets/icon.png");

    Set_Config_Flags(FLAG_WINDOW_RESIZABLE);
    Init_Window(1600, 900, Title);
    Set_Window_Icon(Icon);
    Set_Target_FPS(60);
    Set_Exit_Key(KEY_NULL);

    Init_Audio_Device;
    for Index in Footsteps_Range loop
        Footsteps_Sounds(Index) := Load_Sound(To_C("assets/sounds/footsteps.mp3"));
        Set_Sound_Pitch(Footsteps_Sounds(Index), Footsteps_Pitches(Index));
    end loop;
    Blast_Sound := Load_Sound(To_C("assets/sounds/blast.ogg"));             -- https://opengameart.org/content/magic-sfx-sample
    Key_Pickup_Sound := Load_Sound(To_C("assets/sounds/key-pickup.wav"));   -- https://opengameart.org/content/beep-tone-sound-sfx
    Ambient_Music := Load_Music_Stream(To_C("assets/sounds/ambient.wav"));  -- https://opengameart.org/content/ambient-soundtrack
    Set_Music_Volume(Ambient_Music, 0.5);
    Bomb_Pickup_Sound := Load_Sound(To_C("assets/sounds/bomb-pickup.ogg")); -- https://opengameart.org/content/pickupplastic-sound
    Open_Door_Sound := Load_Sound(To_C("assets/sounds/open-door.wav"));     -- https://opengameart.org/content/picked-coin-echo
    Set_Sound_Volume(Open_Door_Sound, 0.5);
    Checkpoint_Sound := Load_Sound(To_C("assets/sounds/checkpoint.ogg"));   -- https://opengameart.org/content/level-up-power-up-coin-get-13-sounds
    Set_Sound_Pitch(Checkpoint_Sound, 0.8);
    Guard_Step_Sound := Load_Sound(To_C("assets/sounds/guard-step.ogg"));   -- https://opengameart.org/content/fire-whip-hit-yo-frankie
    Plant_Bomb_Sound := Load_Sound(To_C("assets/sounds/plant-bomb.wav"));   -- https://opengameart.org/content/ui-soundpack-by-m1chiboi-bleeps-and-clicks
    Popup_Show_Sound := Load_Sound(To_C("assets/sounds/popup-show.wav"));   -- https://opengameart.org/content/ui-soundpack-by-m1chiboi-bleeps-and-clicks
    Tutorial_Font := Load_Font_Ex(To_C("assets/fonts/Vollkorn/static/Vollkorn-Regular.ttf"), Tutorial_Font_Size, 0, 0);
    Gen_Texture_Mipmaps(Tutorial_Font.Texture'Access);
    Death_Font := Load_Font_Ex(To_C("assets/fonts/Vollkorn/static/Vollkorn-Regular.ttf"), Death_Font_Size, 0, 0);
    Gen_Texture_Mipmaps(Death_Font.Texture'Access);

    Random_Integer.Reset(Gen);
    Load_Colors("assets/colors.txt");
    Load_Game_From_Image("assets/map.png", Game, Update_Player => True, Update_Camera => True);
    Game_Save_Checkpoint(Game);
    Play_Music_Stream(Ambient_Music);

    while not Window_Should_Close loop
        if Is_Music_Stream_Playing(Ambient_Music) then
            Update_Music_Stream(Ambient_Music);
        end if;
        Begin_Drawing;
            Clear_Background(Palette_RGB(COLOR_BACKGROUND));

            Holding_Shift := Boolean(Is_Key_Down(KEY_LEFT_SHIFT)) or else Boolean(Is_Key_Down(KEY_RIGHT_SHIFT));
            if Game.Player.Dead then
                Command_Queue.Size := 0;
            else
                if Holding_Shift and then Game.Turn_Animation <= 0.0 then
                    if Is_Key_Down(KEY_A) or else Is_Key_Down(KEY_LEFT) then
                        Command_Queue.Size := 0;
                        Command_Enqueue(Command_Queue, (Kind => Command_Step, Dir => Left));
                    end if;
                    if Is_Key_Down(KEY_D) or else Is_Key_Down(KEY_RIGHT) then
                        Command_Queue.Size := 0;
                        Command_Enqueue(Command_Queue, (Kind => Command_Step, Dir => Right));
                    end if;
                    if Is_Key_Down(KEY_S) or else Is_Key_Down(KEY_DOWN) then
                        Command_Queue.Size := 0;
                        Command_Enqueue(Command_Queue, (Kind => Command_Step, Dir => Down));
                    end if;
                    if Is_Key_Down(KEY_W) or else Is_Key_Down(KEY_UP) then
                        Command_Queue.Size := 0;
                        Command_Enqueue(Command_Queue, (Kind => Command_Step, Dir => Up));
                    end if;
                else
                    if Is_Key_Pressed(KEY_A) or else Is_Key_Pressed(KEY_LEFT) then
                        Command_Enqueue(Command_Queue, (Kind => Command_Step, Dir => Left));
                    end if;
                    if Is_Key_Pressed(KEY_D) or else Is_Key_Pressed(KEY_RIGHT) then
                        Command_Enqueue(Command_Queue, (Kind => Command_Step, Dir => Right));
                    end if;
                    if Is_Key_Pressed(KEY_S) or else Is_Key_Pressed(KEY_DOWN) then
                        Command_Enqueue(Command_Queue, (Kind => Command_Step, Dir => Down));
                    end if;
                    if Is_Key_Pressed(KEY_W) or else Is_Key_Pressed(KEY_UP) then
                        Command_Enqueue(Command_Queue, (Kind => Command_Step, Dir => Up));
                    end if;
                end if;
                if Is_Key_Pressed(KEY_SPACE) then
                    Command_Enqueue(Command_Queue, (Kind => Command_Plant));
                end if;
            end if;
            if Holding_Shift then
                TURN_DURATION_SECS := BASE_TURN_DURATION_SECS * 0.8;
            else
                if Command_Queue.Size /= 0 then
                    TURN_DURATION_SECS := BASE_TURN_DURATION_SECS * (1.0 / Float(Command_Queue.Size));
                else
                    TURN_DURATION_SECS := BASE_TURN_DURATION_SECS;
                end if;
            end if;

            if DEVELOPMENT then
                if Is_Key_Pressed(KEY_R) then
                    Load_Game_From_Image("assets/map.png", Game, Update_Player => False, Update_Camera => False);
                end if;

                if Is_Key_Pressed(KEY_O) then
                    Palette_Editor := not Palette_Editor;
                    if not Palette_Editor then
                        Save_Colors("assets/colors.txt");
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
            Begin_Mode2D(Game.Camera);
                Game_Cells(Game);
                Game_Items(Game);
                Game_Player(Game);
                Game_Eepers(Game);
                Game_Bombs(Game);
                Game_Tutorial(Game);
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

--  TODO: Items in HUD may sometimes blend with the background
--  TODO: Different palettes on each NG+
--  TODO: NG+ must make the Game harder while retaining the collected bomb slots
--  TODO: The gnome blocking trick was never properly explained.
--    We should introduce an extra room that entirely relies on that mechanic,
--    so it does not feel out of place, when you discover it on Mother.
--  TODO: The puzzle with Gnome blocking Guard repeated twice (which sucks)
--  TODO: Footstep variation for Mother/Guard bosses (depending on the distance traveled?)
--  TODO: Footsteps for mother should be lower
--  TODO: Restarting should be considered a turn
--    It's very useful to update Path Maps and stuff.
--    Or maybe we should just save Path Maps too?
--  TODO: If you are standing on the refilled bomb gen and place a bomb you should refill your bomb in that turn.
--  TODO: Checkpoints should be circles (like all the items)
--  TODO: Mother should require several attacks before being "split"
--  TODO: Enemies should attack on zero just like a bomb.
--  TODO: Properly disablable DEV features
--  TODO: Fullscreen mode
--  TODO: Try MSAA (if too slow, don't)
--  TODO: Show Eeper Cooldown timer outside of the screen somehow
--  TODO: Visual Clue that the Eeper is about to kill the Player when Completely outside of the Screen
--    - Cooldown ball is shaking
--  TODO: Cool animation for New Game
--  TODO: Count the player's turns towards the final score of the game
--    We can even collect different stats, like bombs collected, bombs used,
--    times died etc.
--  TODO: Animate key when you pick it up
--    Smoothly move it into the HUD.
--  TODO: Particles
--    - Player Death animation
--    - Eeper Death animation
--    - Cool effects when you pick up items and checkpoints
--  TODO: Camera shaking when big bosses (Guard and Mother) make moves
--  TODO: Menu
--  TODO: WebAssembly build
--    https://blog.adacore.com/use-of-gnat-llvm-to-translate-ada-applications-to-webassembly
--  TODO: Explosions should trigger other primed bombs?
--  TODO: Path finding in a separate thread
--  TODO: Eeper slide attack animation is pretty boring @polish
--  TODO: Background is too boring
--    Maybe some sort of repeating pattern would be better.
--  TODO: Indicate how many bomb slots we have in HUD
--  TODO: Eyes of Father changing as the Player gets closer:
--    - Happy (very important to indicate that he's not hostile)
