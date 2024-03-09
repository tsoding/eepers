with Text_IO; use Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
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
    COLOR_BACKGROUND : constant Color := Get_Color(16#0b1424ff#);
    COLOR_FLOOR      : constant Color := Get_Color(16#2f2f2fFF#);
    COLOR_WALL       : constant Color := Get_Color(16#000000FF#);
    COLOR_PLAYER     : constant Color := Get_Color(16#3e89ffff#);
    COLOR_DOOR       : constant Color := Get_Color(16#ff9700ff#);
    COLOR_KEY        : constant Color := Get_Color(16#ff9700ff#);
    COLOR_LABEL      : constant Color := Get_Color(16#FFFFFFFF#);

    COLOR_RED        : constant Color := Get_Color(16#FF0000FF#);
    COLOR_PURPLE     : constant Color := Get_Color(16#FF00FFFF#);

    type Cell is (None, Floor, Wall, Barricade, Door, Explosion);
    Width : constant Integer := 20;
    Height : constant Integer := 10;
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

    type IVector2 is record
        X, Y: Integer;
    end record;

    function "="(A, B: IVector2) return Boolean is
    begin
        return A.X = B.X and then A.Y = B.Y;
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

    type Item_Kind is (Key, Bomb);

    package Hashed_Map_Items is new
        Ada.Containers.Hashed_Maps(
            Key_Type => IVector2,
            Element_Type => Item_Kind,
            Hash => Hash_IVector2,
            Equivalent_Keys => Equivalent_IVector2);

    function To_Vector2(iv: IVector2) return Vector2 is
    begin
        return (X => C_float(iv.X), Y => C_float(iv.Y));
    end;

    type Player_State is record
        Position: IVector2;
        Keys: Integer := 0;
        Bombs: Integer := 0;
        Dead: Boolean := False;
    end record;

    type Bomb_State is record
        Position: IVector2;
        Countdown: Integer := 0;
    end record;

    type Bomb_State_Array is array (1..10) of Bomb_State;

    type Game_State is record
        Map: Map_Access := Null;
        Player: Player_State;
        Items: Hashed_Map_Items.Map;
        Bombs: Bomb_State_Array;
        Camera_Position: Vector2 := (x => 0.0, y => 0.0);
        Camera_Velocity: Vector2 := (x => 0.0, y => 0.0);
    end record;

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
                Line: String := Get_Line(F);
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
                Map_Row: Unbounded_String := Map_Rows(Row - 1);
            begin
                Put_Line(To_String(Map_Rows(Row - 1)));
                for Column in Game.Map'Range(2) loop
                    if Column in 1..Length(Map_Row) then
                        case Element(Map_Row, Column) is
                            when '.' => Game.Map(Row, Column) := Floor;
                            when '#' => Game.Map(Row, Column) := Wall;
                            when '=' => Game.Map(Row, Column) := Door;
                            when '*' =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), Bomb);
                            when '&' =>
                                Game.Map(Row, Column) := Barricade;
                            when '%' =>
                                Game.Map(Row, Column) := Floor;
                                Game.Items.Insert((Column, Row), Key);
                            when '@' =>
                                Game.Map(Row, Column) := Floor;
                                if Update_Player then
                                    Game.Player.Position.X := Column;
                                    Game.Player.Position.Y := Row;
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

    procedure Draw_Bomb(Position: IVector2) is
    begin
        Draw_Circle_V(To_Vector2(Position)*Cell_Size + Cell_Size*0.5, Cell_Size.X*0.5, COLOR_RED);
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
                    Position: Vector2 := To_Vector2((Column, Row))*Cell_Size;
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
            case Element(C) is
               when Key => Draw_Key(Key(C));
               when Bomb => Draw_Bomb(Key(C));
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
        Step(Dir, Game.Player.Position);
        case Game.Map(Game.Player.Position.Y, Game.Player.Position.X) is
           when Floor =>
               declare
                   use Hashed_Map_Items;
                   C: Cursor := Game.Items.Find(Game.Player.Position);
               begin
                   if Has_Element(C) then
                       case Element(C) is
                          when Key =>
                              Game.Player.Keys := Game.Player.Keys + 1;
                          when Bomb =>
                              Game.Player.Bombs := Game.Player.Bombs + 1;
                       end case;
                       Game.Items.Delete(C);
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

    procedure Explode(Game: in out Game_State; Position: in IVector2) is
        procedure Explode_Line(Dir: Direction) is
            New_Position: IVector2 := Position;
        begin
            Line: loop
                if New_Position = Game.Player.Position then
                    Game.Player.Dead := True;
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

    Keys: array (Direction) of int := (
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
        Camera_Target: Vector2 :=
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

    procedure Game_Player(Game: in out Game_State) is
    begin
        if Game.Player.Dead then
            if Is_Key_Pressed(KEY_SPACE) then
                --  TODO: better way to reset the state of the game
                --  Introduce the system of checkpoints
                Load_Game_From_File("map.txt", Game, Update_Player => True);
                Game.Player.Dead := False;
            end if;

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
                        Draw_Bomb(Position);
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
                end if;
            end loop;
        end if;
    end;

    procedure Game_Bombs(Game: Game_State) is
    begin
        for Bomb of Game.Bombs loop
            if Bomb.Countdown > 0 then
                Draw_Bomb(Bomb.Position);
                declare
                    Label: Char_Array := To_C(Trim(Integer'Image(Bomb.Countdown), Ada.Strings.Left));
                    Label_Height: Integer := 32;
                    Label_Width: Integer := Integer(Measure_Text(Label, Int(Label_Height)));
                    Text_Size: Vector2 := To_Vector2((Label_Width, Label_Height));
                    Position: Vector2 := To_Vector2(Bomb.Position)*Cell_Size + Cell_Size*0.5 - Text_Size*0.5;
                begin
                    Draw_Text(Label, Int(Position.X), Int(Position.Y), Int(Label_Height), COLOR_LABEL);
                end;
            end if;
        end loop;
    end;

    procedure Game_Hud(Game: in Game_State) is
    begin
        for Index in 1..Game.Player.Keys loop
            declare
                Position: Vector2 := (100.0 + C_float(Index - 1)*Cell_Size.X, 100.0);
            begin
                Draw_Circle_V(Position, Cell_Size.X*0.25, COLOR_KEY);
            end;
        end loop;

        if Game.Player.Dead then
            declare
                Label: Char_Array := To_C("Ded");
                Label_Height: Integer := 48;
                Label_Width: Integer := Integer(Measure_Text(Label, Int(Label_Height)));
                Text_Size: Vector2 := To_Vector2((Label_Width, Label_Height));
                Position: Vector2 := Screen_Size*0.5 - Text_Size*0.5;
            begin
                Draw_Text(Label, Int(Position.X), Int(Position.Y), Int(Label_Height), COLOR_LABEL);
            end;
        end if;
    end;

    Game: Game_State;
    Title: Char_Array := To_C("Hello, NSA");
begin
    Load_Game_From_File("map.txt", Game, True);
    Put_Line("Keys: " & Integer'Image(Game.Player.Keys));
    Set_Config_Flags(FLAG_WINDOW_RESIZABLE);
    Init_Window(800, 600, Title);
    while Window_Should_Close = 0 loop
        Begin_Drawing;
            Clear_Background(COLOR_BACKGROUND);

            if Is_Key_Pressed(KEY_R) then
                Load_Game_From_File("map.txt", Game, False);
            end if;

            Game_Update_Camera(Game);
            Begin_Mode2D(Game_Camera(Game));
                Game_Cells(Game);
                Game_Items(Game);
                Game_Player(Game);
                Game_Bombs(Game);
            End_Mode2D;

            Game_Hud(Game);
        End_Drawing;
    end loop;
    Close_Window;
end;
