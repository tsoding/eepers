with Text_IO; use Text_IO;
with Raylib; use Raylib;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

function Test return Integer is
begin
    Put_Line("Working Directory: " & To_Ada(Value(Get_Working_Directory)));
    Put_Line("Application Directory: " & To_Ada(Value(Get_Application_Directory)));
    return 0;
end;
