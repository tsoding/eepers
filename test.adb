with Ada.Text_IO;
with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test is
    Map: array (1..10, 1..20) of Integer := [others => [others => 0]];
    Index: array (1..2) of Integer;
begin
    if Index in Map then
        Map() := 1;
    end if;
end;
