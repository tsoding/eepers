with Text_IO; use Text_IO;
with Raylib; use Raylib;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

function Test return Integer is
    function Clamp(X, Lo, Hi: Float) return Float is
    begin
        if X < Lo then
            return Lo;
        elsif X > Hi then
            return Hi;
        else
            return X;
        end if;
    end;

    function Repeat(T, Length: Float) return Float is
        function Floorf(A: C_Float) return C_Float
            with
                Import => True,
                Convention => C,
                External_Name => "floorf";
    begin
        return Clamp(T - Float(Floorf(C_Float(T/Length)))*Length, 0.0, Length);
    end;

    function Delta_Angle(A, B: Float) return Float is
        Dlt: Float := Repeat(B - A, 360.0);
    begin
        if Dlt > 180.0 then
            Dlt := Dlt - 360.0;
        end if;
        return Dlt
    end;
begin
    Put_Line(Float'Image(Repeat(-1.0, 360.0)));
    return 0;
end;
