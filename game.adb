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

procedure Game is
    DEVELOPMENT : constant Boolean := True;

    COLOR_BACKGROUND : constant Color := Get_Color(16#0b1424ff#);
    COLOR_FLOOR      : constant Color := Get_Color(16#2f2f2fFF#);
    COLOR_WALL       : constant Color := Get_Color(16#000000FF#);
    COLOR_PLAYER     : constant Color := Get_Color(16#3e89ffff#);
    COLOR_DOOR       : constant Color := Get_Color(16#ff9700ff#);
    COLOR_KEY        : constant Color := Get_Color(16#ff9700ff#);
    COLOR_LABEL      : constant Color := Get_Color(16#FFFFFFFF#);
    COLOR_SHREK      : constant Color := Get_Color(16#97FF00FF#);
    COLOR_CHECKPOINT : constant Color := Get_Color(16#FF00FFFF#);

    COLOR_RED        : constant Color := Get_Color(16#FF0000FF#);
    COLOR_PURPLE     : constant Color := Get_Color(16#FF00FFFF#);

    --  TODO(tool): move this to a hotreloadable config
    TURN_DURATION_SECS      : constant Float := 0.125;
    SHREK_ATTACK_COOLDOWN   : constant Integer := 3;
    SHREK_EXPLOSION_DAMAGE  : constant Float := 0.2;
    SHREK_TURN_REGENERATION : constant Float := 0.01;
    BOMB_GENERATOR_COOLDOWN : constant Integer := 20;

    type IVector2 is record
        X, Y: Integer;
    end record;

    Shrek_Size: constant IVector2 := (3, 3);
    type Cell is (None, Floor, Wall, Barricade, Door, Explosion);
    Cell_Size : constant Vector2 := (x => 50.0, y => 50.0);
    Cell_Colors : constant array (Cell) of Color := (
      None => COLOR_BACKGROUND,
      Floor => COLOR_FLOOR,
      Wall => COLOR_WALL,
      Barricade => COLOR_RED,
      Door => COLOR_DOOR,
      Explosion => COLOR_PURPLE
    );

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

    function "-"(A, B: IVector2) return IVector2 is
    begin
        return (A.X - B.X, A.Y - B.Y);
    end;

    function "+"(A, B: IVector2) return IVector2 is
    begin
        return (A.X + B.X, A.Y + B.Y);
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

    type Item is record
        Kind: Item_Kind;
        Cooldown: Integer;
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

    type Shrek_State is record
        Prev_Position: IVector2;
        Position: IVector2;
        Health: Float := 1.0;
        Attack_Cooldown: Integer := SHREK_ATTACK_COOLDOWN;
        Dead: Boolean;
    end record;

    type Bomb_State is record
        Position: IVector2;
        Countdown: Integer := 0;
    end record;

    type Bomb_State_Array is array (1..10) of Bomb_State;

    type Checkpoint_State is record
        Map: Map_Access := Null;
        Player_Position: IVector2;
        Player_Keys: Integer;
        Player_Bombs: Integer;
        Player_Bomb_Slots: Integer;
        Shrek_Position: IVector2;
        Shrek_Health: Float;
        Shrek_Dead: Boolean;
        Items: Hashed_Map_Items.Map;
    end record;

    type Game_State is record
        Map: Map_Access := Null;
        Player: Player_State;
        Shrek: Shrek_State;

        Turn_Animation: Float := 0.0;

        Items: Hashed_Map_Items.Map;
        Bombs: Bomb_State_Array;
        Camera_Position: Vector2 := (x => 0.0, y => 0.0);
        Camera_Velocity: Vector2 := (x => 0.0, y => 0.0);

        Checkpoint: Checkpoint_State;
    end record;

    function Clone_Map(M0: Map_Access) return Map_Access is
        M1: Map_Access;
    begin
        M1 := new Map(M0'Range(1), M0'Range(2));
        M1.all := M0.all;
        return M1;
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
        Game.Checkpoint.Shrek_Position    := Game.Shrek.Position;
        Game.Checkpoint.Shrek_Dead        := Game.Shrek.Dead;
        Game.Checkpoint.Shrek_Health      := Game.Shrek.Health;
        Game.Checkpoint.Items             := Game.Items;
    end;

    procedure Game_Restore_Checkpoint(Game: in out Game_State) is
    begin
        if Game.Map /= null then
            Delete_Map(Game.Map);
        end if;
        Game.Map := Clone_Map(Game.Checkpoint.Map);
        Game.Player.Position   := Game.Checkpoint.Player_Position;
        Game.Player.Keys       := Game.Checkpoint.Player_Keys;
        Game.Player.Bombs      := Game.Checkpoint.Player_Bombs;
        Game.Player.Bomb_Slots := Game.Checkpoint.Player_Bomb_Slots;
        Game.Shrek.Position    := Game.Checkpoint.Shrek_Position;
        Game.Shrek.Dead        := Game.Checkpoint.Shrek_Dead;
        Game.Shrek.Health      := Game.Checkpoint.Shrek_Health;
        Game.Items             := Game.Checkpoint.Items;
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

        if Game.Map /= Null then
            Delete_Map(Game.Map);
        end if;
        Game.Items.Clear;
        for Bomb of Game.Bombs loop
            Bomb.Countdown := 0;
        end loop;

        Game.Map := new Map(1..Height, 1..Width);
        for Row in Game.Map'Range(1) loop
            declare
                Map_Row: constant Unbounded_String := Map_Rows(Row - 1);
            begin
                Put_Line(To_String(Map_Rows(Row - 1)));
                for Column in Game.Map'Range(2) loop
                    if Column in 1..Length(Map_Row) then
                        case Element(Map_Row, Column) is
                            when 'B' =>
                                Game.Shrek.Position := (Column, Row);
                                Game.Shrek.Prev_Position := (Column, Row);
                                Game.Shrek.Health := 1.0;
                                Game.Shrek.Dead := False;
                                Game.Map(Row, Column) := Floor;
                            when '.' => Game.Map(Row, Column) := Floor;
                            when '#' => Game.Map(Row, Column) := Wall;
                            when '=' => Game.Map(Row, Column) := Door;
                            when '!' =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), (Kind => Checkpoint, Cooldown => 0));
                            when '*' =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), (Kind => Bomb, Cooldown => 0));
                            when '&' =>
                                Game.Map(Row, Column) := Barricade;
                            when '%' =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), (Kind => Key, Cooldown => 0));
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
        Draw_Circle_V(To_Vector2(Position)*Cell_Size + Cell_Size*0.5, Cell_Size.X*0.25, COLOR_KEY);
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
                        Draw_Rectangle_V(To_Vector2(Key(C))*Cell_Size + Cell_Size*0.5 - Checkpoint_Item_Size*0.5, Checkpoint_Item_Size, COLOR_CHECKPOINT);
                    end;
                when Bomb =>
                    if Element(C).Cooldown > 0 then
                        Draw_Bomb(Key(C), Color_Brightness(COLOR_RED, -0.5));
                        Draw_Number(Key(C), Element(C).Cooldown, COLOR_LABEL);
                    else
                        Draw_Bomb(Key(C), COLOR_RED);
                    end if;
            end case;
        end loop;
    end;

    type Direction is (Left, Right, Up, Down);

    procedure Step(D: in Direction; Position: in out IVector2) is
    begin
        case D is
            when Left  => Position.X := Position.X - 1;
            when Right => Position.X := Position.X + 1;
            when Up    => Position.Y := Position.Y - 1;
            when Down  => Position.Y := Position.Y + 1;
        end case;
    end;

    function Opposite(D: Direction) return Direction is
    begin
        case D is
            when Left  => return Right;
            when Right => return Left;
            when Up    => return Down;
            when Down  => return Up;
        end case;
    end;

    procedure Player_Step(Game: in out Game_State; Dir: Direction) is
    begin
        Game.Player.Prev_Position := Game.Player.Position;
        Game.Turn_Animation := 1.0;
        Step(Dir, Game.Player.Position);
        case Game.Map(Game.Player.Position.Y, Game.Player.Position.X) is
           when Floor =>
               declare
                   use Hashed_Map_Items;
                   C: Cursor := Game.Items.Find(Game.Player.Position);
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
           when Door =>
               if Game.Player.Keys > 0 then
                   Game.Player.Keys := Game.Player.Keys - 1;
                   Game.Map(Game.Player.Position.Y, Game.Player.Position.X) := Floor;
               else
                   Step(Opposite(Dir), Game.Player.Position);
               end if;
           when others =>
               Step(Opposite(Dir), Game.Player.Position);
        end case;
    end;

    function Inside_Of_Rect(Start, Size, Point: in IVector2) return Boolean is
    begin
        return Start <= Point and then Point < Start + Size;
    end;

    procedure Explode(Game: in out Game_State; Position: in IVector2) is
        procedure Explode_Line(Dir: Direction) is
            New_Position: IVector2 := Position;
        begin
            Line: for I in 1..10 loop
                if New_Position = Game.Player.Position then
                    Game.Player.Dead := True;
                end if;
                if not Game.Shrek.Dead and then Inside_Of_Rect(Game.Shrek.Position, Shrek_Size, New_Position) then
                    Game.Shrek.Health := Game.Shrek.Health - SHREK_EXPLOSION_DAMAGE;
                    if Game.Shrek.Health <= 0.0 then
                        Game.Shrek.Dead := True;
                    end if;
                end if;
                case Game.Map(New_Position.Y, New_Position.X) is
                   when Floor | Explosion =>
                       Game.Map(New_Position.Y, New_Position.X) := Explosion;
                       Step(Dir, New_Position);
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

    Keys: constant array (Direction) of int := (
        Left  => KEY_A,
        Right => KEY_D,
        Up    => KEY_W,
        Down  => KEY_S
    );

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
        return Prev_Position + (Curr_Position - Prev_Position)*C_Float(1.0 - T);
    end;

    Unreachable: exception;

    function Can_Reach(Game: in Game_State; Start, Finish, Size: IVector2) return Boolean is
        Position: IVector2 := Start;
        Direction: IVector2 := Finish - Start;
    begin
        if Direction.X /= 0 then
            Direction.X := Direction.X/abs(Direction.X);
        elsif Direction.Y /= 0 then
            Direction.Y := Direction.Y/abs(Direction.Y);
        else
            raise Unreachable;
        end if;

        while Position /= Finish loop
            Position := Position + Direction;
            for X in Position.X..Position.X+Size.X-1 loop
                for Y in Position.Y..Position.Y+Size.Y-1 loop
                    if Game.Map(Y, X) = Wall then
                        return False;
                    end if;
                end loop;
            end loop;
        end loop;

        return True;
    end;

    procedure Game_Player(Game: in out Game_State) is
    begin
        if Game.Player.Dead then
            if Is_Key_Pressed(KEY_SPACE) then
                Game_Restore_Checkpoint(Game);
                Game.Player.Dead := False;
            end if;

            return;
        end if;

        if Game.Turn_Animation > 0.0 then
            Draw_Rectangle_V(Interpolate_Positions(Game.Player.Prev_Position, Game.Player.Position, Game.Turn_Animation), Cell_Size, COLOR_PLAYER);
            return;
        end if;

        Draw_Rectangle_V(To_Vector2(Game.Player.Position)*Cell_Size, Cell_Size, COLOR_PLAYER);
        if Boolean(Is_Key_Down(KEY_SPACE)) and then Game.Player.Bombs > 0 then
            for Dir in Direction loop
                declare
                    Position: IVector2 := Game.Player.Position;
                begin
                    Step(Dir, Position);
                    if Game.Map(Position.Y, Position.X) = Floor then
                        Draw_Bomb(Position, COLOR_RED);
                        if Is_Key_Pressed(Keys(Dir)) then
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
                if Is_Key_Pressed(Keys(Dir)) then
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

                    if not Game.Shrek.Dead then
                        Game.Shrek.Prev_Position := Game.Shrek.Position;
                        -- TODO: Shrek should attack on zero just like a bomb.
                        if Game.Shrek.Attack_Cooldown <= 0 then
                            declare
                                Delta_Pos: constant IVector2 := Game.Player.Position - Game.Shrek.Position;
                                
                                procedure Try_Move_Shrek_To(New_Position: IVector2) is
                                begin
                                    if Can_Reach(Game, Game.Shrek.Position, New_Position, Shrek_Size) then
                                        Game.Shrek.Position := New_Position;
                                    end if;
                                end;

                                procedure Shrek_Try_Attack_Horizontally is
                                begin
                                    if Delta_Pos.X < 0 then
                                        Try_Move_Shrek_To((Game.Player.Position.X, Game.Shrek.Position.Y));
                                    else
                                        Try_Move_Shrek_To((Game.Player.Position.X - Shrek_Size.X + 1, Game.Shrek.Position.Y));
                                    end if;
                                end;

                                procedure Shrek_Try_Attack_Vertically is
                                begin
                                    if Delta_Pos.Y < 0 then
                                        Try_Move_Shrek_To((Game.Shrek.Position.X, Game.Player.Position.Y));
                                    else 
                                        Try_Move_Shrek_To((Game.Shrek.Position.X, Game.Player.Position.Y - Shrek_Size.Y + 1));
                                    end if;
                                end;
                            begin
                                if Delta_Pos.X in 0..3-1 then
                                    Put_Line("Align_Shrek_Vertically");
                                    Shrek_Try_Attack_Vertically;
                                elsif Delta_Pos.Y in 0..3-1 then
                                    Put_Line("Align_Shrek_Horizontally");
                                    Shrek_Try_Attack_Horizontally;
                                else
                                    Put_Line("Diagonal");
                                    if abs(Delta_Pos.X) < abs(Delta_Pos.Y) then
                                        Put_Line("Diagonal: Align_Shrek_Horizontally");
                                        Shrek_Try_Attack_Horizontally;
                                    else
                                        Put_Line("Diagonal: Align_Shrek_Vertial");
                                        Shrek_Try_Attack_Vertically;
                                    end if;
                                end if;
                            end;
                            Game.Shrek.Attack_Cooldown := SHREK_ATTACK_COOLDOWN;
                        else
                            Game.Shrek.Attack_Cooldown := Game.Shrek.Attack_Cooldown - 1;
                        end if;
                        if Inside_Of_Rect(Game.Shrek.Position, Shrek_Size, Game.Player.Position) then
                            Game.Player.Dead := True;
                        end if;
                        if Game.Shrek.Health < 1.0 then
                           Game.Shrek.Health := Game.Shrek.Health + SHREK_TURN_REGENERATION;
                        end if;
                    end if;
                end if;
            end loop;
        end if;
    end;

    procedure Game_Bombs(Game: Game_State) is
    begin
        for Bomb of Game.Bombs loop
            if Bomb.Countdown > 0 then
                Draw_Bomb(Bomb.Position, COLOR_RED);
                Draw_Number(Bomb.Position, Bomb.Countdown, COLOR_LABEL);
            end if;
        end loop;
    end;

    procedure Game_Hud(Game: in Game_State) is
    begin
        for Index in 1..Game.Player.Keys loop
            declare
                Position: constant Vector2 := (100.0 + C_float(Index - 1)*Cell_Size.X, 100.0);
            begin
                Draw_Circle_V(Position, Cell_Size.X*0.25, COLOR_KEY);
            end;
        end loop;

        for Index in 1..Game.Player.Bombs loop
            declare
                Position: constant Vector2 := (100.0 + C_float(Index - 1)*Cell_Size.X, 200.0);
            begin
                Draw_Circle_V(Position, Cell_Size.X*0.5, COLOR_RED);
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
                Draw_Text(Label, Int(Position.X), Int(Position.Y), Int(Label_Height), COLOR_LABEL);
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
          COLOR_RED);
    end;

    procedure Game_Shrek(Game: in out Game_State) is
        Position: constant Vector2 :=
          (if Game.Turn_Animation > 0.0
           then Interpolate_Positions(Game.Shrek.Prev_Position, Game.Shrek.Position, Game.Turn_Animation)
           else To_Vector2(Game.Shrek.Position)*Cell_Size);
        Size: constant Vector2 := To_Vector2(Shrek_Size)*Cell_Size;
    begin
        if Game.Shrek.Dead then
            return;
        end if;
        Draw_Rectangle_V(Position, Cell_Size*3.0, COLOR_SHREK);
        Health_Bar(Position, Size, C_Float(Game.Shrek.Health));
        Draw_Number(Position, Size, Game.Shrek.Attack_Cooldown, (A => 255, others => 0));
    end;

    Game: Game_State;
    Title: constant Char_Array := To_C("Hello, NSA");
begin
    Load_Game_From_File("map.txt", Game, True);
    Game_Save_Checkpoint(Game);
    Put_Line("Keys: " & Integer'Image(Game.Player.Keys));
    Set_Config_Flags(FLAG_WINDOW_RESIZABLE);
    Init_Window(800, 600, Title);
    Set_Target_FPS(60);
    while Window_Should_Close = 0 loop
        Begin_Drawing;
            Clear_Background(COLOR_BACKGROUND);

            if DEVELOPMENT then
                if Is_Key_Pressed(KEY_R) then
                    Load_Game_From_File("map.txt", Game, False);
                    Game_Save_Checkpoint(Game);
                end if;

                --  TODO(tool): implement the palette editor
                --  TODO(tool): save current checkpoint to file for debug purposes
            end if;

            if Game.Turn_Animation > 0.0 then
                Game.Turn_Animation := (Game.Turn_Animation*TURN_DURATION_SECS - Float(Get_Frame_Time))/TURN_DURATION_SECS;
            end if;

            Game_Update_Camera(Game);
            Begin_Mode2D(Game_Camera(Game));
                Game_Cells(Game);
                Game_Items(Game);
                Game_Player(Game);
                Game_Shrek(Game);
                Game_Bombs(Game);
            End_Mode2D;

            Game_Hud(Game);
            Draw_FPS(10, 10);
        End_Drawing;
    end loop;
    Close_Window;
end;

--  TODO: mechanics to skip a turn
--  TODO: placing a bomb is not a turn (should it be tho?)
--  TODO: tutorial does not "explain" how to place bomb
--  TODO: keep steping while you are holding a certain direction
--    Cause constantly tapping it feels like ass
--  TODO: count the player's turns towards the final score of the game
--    We can even collect different stats, like bombs collected, bombs used,
--    times deid etc.
