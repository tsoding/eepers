with Ada.Text_IO;
with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;

procedure Test is
    type Direction is (Left, Right, Up, Down);
    type Array_Direction is array(Direction) of Integer;
    --  type Direction is range 1..10;
begin
    Put_Line(Direction'First'Image);
    Put_Line(Direction'Last'Image);
    Put_Line(Array_Direction'Length'Image);
end;
