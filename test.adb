with Ada.Text_IO;
with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test is
    type State is record
        X: Integer;
        Y: Integer;
    end record;

    procedure Foo(S: in State; V: out Integer) is
    begin
        V := S.X*2;
    end;

    S: State := (X => 69, Y => 420);
begin
    Put_Line(S'Image);
    Foo(S, S.Y);
    Put_Line(S'Image);
end;
