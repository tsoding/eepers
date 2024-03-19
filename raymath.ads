with Interfaces.C; use Interfaces.C;

package Raymath is
    type Vector2 is record
        x: C_float;
        y: C_float;
    end record
        with Convention => C_Pass_By_Copy;
    function "+"(a, b: Vector2) return Vector2
        with
            Import => True,
            Convention => C,
            External_Name => "Vector2Add";
    function "-"(a, b: Vector2) return Vector2
        with
            Import => True,
            Convention => C,
            External_Name => "Vector2Subtract";
    function "*"(a, b: Vector2) return Vector2
        with
            Import => True,
            Convention => C,
            External_Name => "Vector2Multiply";
    function "*"(v: Vector2; scale: C_float) return Vector2
        with
            Import => True,
            Convention => C,
            External_Name => "Vector2Scale";
    function Vector2_Rotate(V: Vector2; Angle: C_Float) return Vector2
        with
            Import => True,
            Convention => C,
            External_Name => "Vector2Rotate";
    function Vector2_Line_Angle(Start, Finish: Vector2) return C_Float
        with
            Import => True,
            Convention => C,
            External_Name => "Vector2LineAngle";
    type Vector3 is record
        x: C_float;
        y: C_float;
        z: C_float;
    end record
        with Convention => C_Pass_By_Copy;
end;
