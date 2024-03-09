with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;

procedure Test is
    type IVector2 is record
        X, Y: Integer;
    end record;
    type Ten_Numbers is array(IVector2) of Integer;

    Position: IVector2;
    Xs: Ten_Numbers;
begin
    null;
end;
