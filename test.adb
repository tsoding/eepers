with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;


procedure Test is
begin
   for Index in 1..10 loop
      Put_Line(Trim(Integer'Image(Index), Left));
   end loop;
end;
