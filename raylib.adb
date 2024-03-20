package body Raylib is
    procedure Draw_Triangle_Strip(Points: Vector2_Array; C: Color) is
        procedure Draw_Triangle_Strip_C(Points: Vector2_Array; Point_Count: Int; C: Color)
          with
            Import => True,
            Convention => C,
            External_Name => "DrawTriangleStrip";
    begin
        Draw_Triangle_Strip_C(Points, Points'Length, C);
    end;

end Raylib;
