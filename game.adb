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

procedure Game is
    COLOR_BACKGROUND : constant Color := Get_Color(16#0b1424ff#);
    COLOR_FLOOR      : constant Color := Get_Color(16#2f2f2fFF#);
    COLOR_WALL       : constant Color := Get_Color(16#000000FF#);
    COLOR_PLAYER     : constant Color := Get_Color(16#3e89ffff#);
    COLOR_RED        : constant Color := Get_Color(16#FF0000FF#);
    COLOR_DOOR       : constant Color := Get_Color(16#ff9700ff#);
    COLOR_KEY        : constant Color := Get_Color(16#ff9700ff#);

    type Cell is (None, Floor, Wall, Barricade, Door);
    Width : constant Integer := 20;
    Height : constant Integer := 10;
    Cell_Size : constant Vector2 := (x => 50.0, y => 50.0);
    Cell_Colors : constant array (Cell) of Color := (
      None => COLOR_BACKGROUND,
      Floor => COLOR_FLOOR,
      Wall => COLOR_WALL,
      Barricade => COLOR_RED,
      Door => COLOR_DOOR
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
    end record;

    type Game_State is record
        Map: Map_Access := Null;
        Player: Player_State;
        Items: Hashed_Map_Items.Map;
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

    procedure Draw_Game(Game: in Game_State) is
    begin
        for Row in Game.Map'Range(1) loop
            for Column in Game.Map'Range(2) loop
                declare
                    Position: Vector2 := To_Vector2((Column, Row))*Cell_Size;
                    C: Color :=
                      (if (Column, Row) = Game.Player.Position
                       then COLOR_PLAYER
                       else Cell_Colors(Game.Map(Row, Column)));
                begin
                    Draw_Rectangle_V(position, cell_size, C);
                end;
            end loop;
        end loop;

        declare
            use Hashed_Map_Items;
        begin
            for C in Game.Items.Iterate loop
                case Element(C) is
                    when Key => Draw_Key(Key(C));
                    when Bomb => Draw_Bomb(Key(C));
                end case;
            end loop;
        end;
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

    Keys: array (Direction) of int := (
        Left  => KEY_A,
        Right => KEY_D,
        Up    => KEY_W,
        Down  => KEY_S
    );

    Game: Game_State;
begin
    Load_Game_From_File("map.txt", Game, True);
    Put_Line("Keys: " & Integer'Image(Game.Player.Keys));
    Set_Config_Flags(FLAG_WINDOW_RESIZABLE);
    Init_Window(800, 600, New_String("Hello, NSA"));
    while Window_Should_Close = 0 loop
        Begin_Drawing;
            Clear_Background(COLOR_BACKGROUND);
            if Is_Key_Pressed(KEY_R) then
                Load_Game_From_File("map.txt", Game, False);
            end if;
            Game.Camera_Position := Game.Camera_Position + Game.Camera_Velocity*Get_Frame_Time;
            declare
                camera : Camera2D := (
                    offset => Game.Camera_Position,
                    target => (x => 0.0, y => 0.0),
                    rotation => 0.0,
                    zoom => 1.0
                );
                Screen_Size: Vector2 := To_Vector2((Integer(Get_Screen_Width), Integer(Get_Screen_Height)));
                Camera_Target: Vector2 :=
                    Screen_Size*0.5 - To_Vector2(Game.Player.Position)*Cell_Size - Cell_Size*0.5;
            begin
                Game.Camera_Velocity := (Camera_Target - Game.Camera_Position)*2.0;
                Begin_Mode2D(Camera);
                    Draw_Game(Game);
                    if Boolean(Is_Key_Down(KEY_SPACE)) and then Game.Player.Bombs > 0 then
                        for Dir in Direction loop
                            declare
                                Side: IVector2 := Game.Player.Position;
                            begin
                                Step(Dir, Side);
                                if Game.Map(Side.Y, Side.X) = Floor then
                                    Draw_Bomb(Side);
                                    if Is_Key_Pressed(Keys(Dir)) then
                                        Game.Map(Side.Y, Side.X) := Wall;
                                        Game.Player.Bombs := Game.Player.Bombs - 1;
                                    end if;
                                end if;
                            end;
                        end loop;
                    else
                        for Dir in Direction loop
                            if Is_Key_Pressed(Keys(Dir)) then
                                Player_Step(Game, Dir);
                            end if;
                        end loop;
                    end if;
                End_Mode2D;
                for Index in 1..Game.Player.Keys loop
                    declare
                        Position: Vector2 := (100.0 + C_float(Index - 1)*Cell_Size.X, 100.0);
                    begin
                        Draw_Circle_V(Position, Cell_Size.X*0.25, COLOR_KEY);
                    end;
                end loop;
            end;
        End_Drawing;
    end loop;
    Close_Window;
end;
