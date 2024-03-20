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
with Raylib; use Raylib;
with Raymath; use Raymath;

function Test return Integer is
    type Vector2_Array is array (size_t range <>) of aliased Vector2;
    procedure Draw_Triangle_Strip(Points: Vector2_Array; C: Color) is
        procedure Draw_Triangle_Strip_C(Points: Vector2_Array; Point_Count: Int; C: Color)
          with
            Import => True,
            Convention => C,
            External_Name => "DrawTriangleStrip";
    begin
        Draw_Triangle_Strip_C(Points, Points'Length, C);
    end;

    Size: Vector2;
    Radius: constant C_Float := 100.0;
begin
    Init_Window(800, 600, To_C("Test"));
    Set_Target_FPS(60);
    while not Window_Should_Close loop
        Begin_Drawing;
            Size := (C_Float(Get_Screen_Width), C_Float(Get_Screen_Height));
            Clear_Background(Get_Color(16#181818FF#));
            Draw_Triangle_Strip(
              Points => [
                  Size*0.5 + (-Radius, -Radius),
                  Size*0.5 + (-Radius, Radius),
                  Size*0.5 + (Radius, -Radius),
                  Size*0.5 + (Radius, Radius)
              ],
              C => (R => 255, A => 255, others => 0));
        End_Drawing;
    end loop;
    Close_Window;
    return 0;
end;
