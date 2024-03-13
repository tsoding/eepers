with Ada.Text_IO;
with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test is
    File_Name: constant String := "colors.txt";
    F: File_Type;

    function Chop_By(Src: in out Unbounded_String; Pattern: String) return Unbounded_String is
        Space_Index: Integer := Index(Src, Pattern);
        Result: Unbounded_String;
    begin
        if Space_Index = 0 then
            Result := Src;
            Src := Null_Unbounded_String;
        else
            Result := Unbounded_Slice(Src, 1, Space_Index - 1);
            Src := Unbounded_Slice(Src, Space_Index + 1, Length(Src));
        end if;

        return Result;
    end;
begin
    Open(F, In_File, File_Name);
    while not End_Of_File(F) loop
        declare
            Line: Unbounded_String := To_Unbounded_String(Get_Line(F));
        begin
            Line := Trim(Line, Ada.Strings.Left);
            Put_Line('"' & To_String(Chop_By(Line, " ")) & '"');

            for Times in 1..4 loop
                Line := Trim(Line, Ada.Strings.Left);
                declare
                    Token: Unbounded_String := Chop_By(Line, " ");
                begin
                    --  Put_Line(Float'Value(To_String(Token))'Image);
                    Put_Line(To_String(Token));
                end;
            end loop;
        end;
        Put_Line("------------------------------");
    end loop;
    Close(F);
end;
