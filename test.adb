with Ada.Text_IO;
with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Raylib; use Raylib;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Ada.Unchecked_Conversion;

function Test return Integer is
    type Color_Array is array (Natural range <>) of aliased Raylib.Color;
    package Color_Pointer is new Interfaces.C.Pointers(
      Index => Natural,
      Element => Raylib.Color,
      Element_Array => Color_Array,
      Default_Terminator => (others => 0));
    function To_Color_Pointer is new Ada.Unchecked_Conversion (Raylib.Addr, Color_Pointer.Pointer);
    use Color_Pointer;

    function Load_Text_As_Image(File_Name: in String) return Raylib.Image is
        package Rows is new
            Ada.Containers.Vectors(
                Index_Type => Natural,
                Element_Type => Unbounded_String);
        F: File_Type;
        Map_Rows: Rows.Vector;
        Width: Integer := 0;
        Height: Integer := 0;
        Img: Raylib.Image;
        Pixels: Color_Pointer.Pointer;
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

        Img := Raylib.Gen_Image_Color(Int(Width), Int(Height), (others => 0));
        Pixels := To_Color_Pointer(Img.Data);

        for Row in 1..Height loop
            declare
                Map_Row: constant Unbounded_String := Map_Rows(Row - 1);
            begin
                Put_Line(To_String(Map_Rows(Row - 1)));
                for Column in 1..Width loop
                    declare
                        Index: Ptrdiff_T := Ptrdiff_T((Row - 1)*Width + (Column - 1));
                        Pixel: Color_Pointer.Pointer := Pixels + Index;
                        BLACK:  constant Raylib.Color := (A => 255, others => 0);
                        WHITE:  constant Raylib.Color := (others => 255);
                        RED:    constant Raylib.Color := (R => 255, A => 255, others => 0);
                        ORANGE: constant Raylib.Color := (R => 255, G => 150, A => 255, others => 0);
                        FOO:    constant Raylib.Color := (R => 255, B => 150, A => 255, others => 0);
                        GREEN:  constant Raylib.Color := (G => 255, A => 255, others => 0);
                        BAR:    constant Raylib.Color := (G => 255, R => 150, A => 255, others => 0);
                        BLUE:   constant Raylib.Color := (B => 255, A => 255, others => 0);
                        CYAN:   constant Raylib.Color := (G => 255, B => 255, A => 255, others => 0);
                        PURPLE: constant Raylib.Color := (R => 255, B => 255, A => 255, others => 0);
                        YELLOW: constant Raylib.Color := (R => 255, G => 255, A => 255, others => 0);
                    begin
                        if Column in 1..Length(Map_Row) then
                            case Element(Map_Row, Column) is
                                when 'G' => Pixel.all := ORANGE;
                                when 'M' => Pixel.all := BAR;
                                when 'B' => Pixel.all := GREEN;
                                when '.' => Pixel.all := WHITE;
                                when '#' => Pixel.all := BLACK;
                                when '=' => Pixel.all := CYAN;
                                when '!' => Pixel.all := PURPLE;
                                when '*' => Pixel.all := RED;
                                when '&' => Pixel.all := FOO;
                                when '%' => Pixel.all := YELLOW;
                                when '@' => Pixel.all := BLUE;
                                when others => Pixel.all := (others => 0);
                            end case;
                        else
                            Pixel.all := (others => 0);
                        end if;
                    end;
                end loop;
            end;
        end loop;
        return Img;
    end;

    type Level_Cell is (
      Level_None,
      Level_Gnome,
      Level_Urmom,
      Level_Shrek,
      Level_Floor,
      Level_Wall,
      Level_Door,
      Level_Checkpoint,
      Level_Bomb_Gen,
      Level_Barricade,
      Level_Key,
      Level_Player);

    Level_Cell_Color: constant array (Level_Cell) of Color := [
      Level_None       => Get_Color(16#00000000#),
      Level_Gnome      => Get_Color(16#FF9600FF#),
      Level_Urmom      => Get_Color(16#96FF00FF#),
      Level_Shrek      => Get_Color(16#00FF00FF#),
      Level_Floor      => Get_Color(16#FFFFFFFF#),
      Level_Wall       => Get_Color(16#000000FF#),
      Level_Door       => Get_Color(16#00FFFFFF#),
      Level_Checkpoint => Get_Color(16#FF00FFFF#),
      Level_Bomb_Gen   => Get_Color(16#FF0000FF#),
      Level_Barricade  => Get_Color(16#FF0096FF#),
      Level_Key        => Get_Color(16#FFFF00FF#),
      Level_Player     => Get_Color(16#0000FFFF#),

    Img: Raylib.Image := Raylib.Load_Image(To_C("glider.png"));
    Pixels: Color_Pointer.Pointer := To_Color_Pointer(Img.Data);
    New_Img: Raylib.Image := Load_Text_As_Image("map.txt");

begin
    for Cell in Level_Cell loop
        Put_Line(Unsigned'Image(Color_To_Int(Level_Cell_Color(Cell))));
    end loop;
    return 0;
end;
