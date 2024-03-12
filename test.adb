with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;

procedure Test is
    package Queue is new
        Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Integer);
    Q: Queue.Vector;
begin
    for Index in 1..10 loop
        Q.Append(Index);
    end loop;
    while not Q.Is_Empty loop
        Put_Line(Integer'Image(Q(0)));
        Q.Delete_First;
    end loop;
end;
