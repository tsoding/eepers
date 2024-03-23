with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Raymath; use Raymath;

package Raylib is
    procedure Init_Window(Width, Height: int; Title: in char_array)
        with
            Import => True,
            Convention => C,
            External_Name => "InitWindow";
    procedure Close_Window
        with
            Import => True,
            Convention => C,
            External_Name => "CloseWindow";
    function Window_Should_Close return C_Bool
        with
            Import => True,
            Convention => C,
            External_Name => "WindowShouldClose";
    procedure Begin_Drawing
        with
            Import => True,
            Convention => C,
            External_Name => "BeginDrawing";
    procedure End_Drawing
        with
            Import => True,
            Convention => C,
            External_Name => "EndDrawing";
    type Color is record
        r: unsigned_char;
        g: unsigned_char;
        b: unsigned_char;
        a: unsigned_char;
    end record
        with Convention => C_Pass_By_Copy;
    procedure Clear_Background(c: Color)
        with
            Import => True,
            Convention => C,
            External_Name => "ClearBackground";
    procedure Draw_Rectangle(posX, posY, Width, Height: int; c: Color)
        with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangle";
    function Get_Screen_Width return int
        with
            Import => True,
            Convention => C,
            External_Name => "GetScreenWidth";
    function Get_Screen_Height return int
        with
            Import => True,
            Convention => C,
            External_Name => "GetScreenHeight";
    FLAG_WINDOW_RESIZABLE: constant unsigned := 16#00000004#;
    procedure Set_Config_Flags(flags: unsigned)
        with
            Import => True,
            Convention => C,
            External_Name => "SetConfigFlags";
    KEY_NULL:   constant int := 0;
    KEY_R:      constant int := 82;
    KEY_S:      constant int := 83;
    KEY_W:      constant int := 87;
    KEY_A:      constant int := 65;
    KEY_D:      constant int := 68;
    KEY_P:      constant int := 80;
    KEY_O:      constant int := 79;
    KEY_X:      constant int := 88;
    KEY_RIGHT:  constant int := 262;
    KEY_LEFT:   constant int := 263;
    KEY_DOWN:   constant int := 264;
    KEY_UP:     constant int := 265;
    KEY_SPACE:  constant int := 32;
    KEY_ESCAPE: constant int := 256;
    KEY_ENTER: constant Int := 257;
    function Is_Key_Pressed(key: int) return C_bool
        with
            Import => True,
            Convention => C,
            External_Name => "IsKeyPressed";
    function Get_Key_Pressed return int
        with
            Import => True,
            Convention => C,
            External_Name => "GetKeyPressed";

    procedure Draw_Rectangle_V(position: Vector2; size: Vector2; c: Color)
        with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleV";

    type Camera2D is record
        offset: Vector2;
        target: Vector2;
        rotation: C_float;
        zoom: C_float;
    end record
        with Convention => C_Pass_By_Copy;
    procedure Begin_Mode2D(camera: Camera2D)
        with
            Import => True,
            Convention => C,
            External_Name => "BeginMode2D";
    procedure End_Mode2D
        with
            Import => True,
            Convention => C,
            External_Name => "EndMode2D";
    function Get_Frame_Time return C_float
        with
            Import => True,
            Convention => C,
            External_Name => "GetFrameTime";
    function Get_Color(hexValue: unsigned) return Color
        with
            Import => True,
            Convention => C,
            External_Name => "GetColor";
    function Color_To_Int(C: Color) return unsigned
        with
            Import => True,
            Convention => C,
            External_Name => "ColorToInt";
    procedure Draw_Circle(centerX, centerY: int; radius: C_float; c: Color)
        with
            Import => True,
            Convention => C,
            External_Name => "DrawCircle";
    procedure Draw_Circle_V(center: Vector2; radius: C_float; C: Color)
        with
            Import => True,
            Convention => C,
            External_Name => "DrawCircleV";
    function Is_Key_Down(Key: int) return C_Bool
        with
            Import => True,
            Convention => C,
            External_Name => "IsKeyDown";
    function Measure_Text(Text: Char_Array; FontSize: Int) return Int
        with
            Import => True,
            Convention => C,
            External_Name => "MeasureText";
    procedure Draw_Text(Text: Char_Array; PosX, PosY: Int; FontSize: Int; C: Color)
        with
            Import => True,
            Convention => C,
            External_Name => "DrawText";
    procedure Set_Target_FPS(Fps: int)
        with
            Import => True,
            Convention => C,
            External_Name => "SetTargetFPS";
    procedure Draw_FPS(PosX, PosY: Int)
        with
            Import => True,
            Convention => C,
            External_Name => "DrawFPS";
    function Color_Brightness(C: Color; Factor: C_Float) return Color
        with
            Import => True,
            Convention => C,
            External_Name => "ColorBrightness";
    function Color_To_HSV(C: Color) return Vector3
        with
            Import => True,
            Convention => C,
            External_Name => "ColorToHSV";
    function Color_From_HSV(Hue, Saturation, Value: C_Float) return Color
        with
            Import => True,
            Convention => C,
            External_Name => "ColorFromHSV";
    procedure Set_Exit_Key(Key: Int)
        with
            Import => True,
            Convention => C,
            External_Name => "SetExitKey";
    function Get_Time return Double
        with
            Import => True,
            Convention => C,
            External_Name => "GetTime";
    type Addr is mod 2 ** Standard'Address_Size;
    type Image is record
        Data: Addr;
        Width: int;
        Height: int;
        Mipmaps: int;
        Format: int;
    end record
        with Convention => C_Pass_By_Copy;
    function Load_Image(File_Name: Char_Array) return Image
        with
            Import => True,
            Convention => C,
            External_Name => "LoadImage";
    function Gen_Image_Color(Width, Height: Int; C: Color) return Image
        with
            Import => True,
            Convention => C,
            External_Name => "GenImageColor";
    function Export_Image(Img: Image; File_Name: Char_Array) return C_Bool
        with
            Import => True,
            Convention => C,
            External_Name => "ExportImage";
    procedure Draw_Triangle(V1, V2, V3: Vector2; C: Color)
        with
            Import => True,
            Convention => C,
            External_Name => "DrawTriangle";

    type Vector2_Array is array (size_t range <>) of aliased Vector2;

    procedure Draw_Triangle_Strip(Points: Vector2_Array; C: Color);

    function Get_Working_Directory return chars_ptr
        with
            Import => True,
            Convention => C,
            External_Name => "GetWorkingDirectory";
    function Get_Application_Directory return chars_ptr
        with
            Import => True,
            Convention => C,
            External_Name => "GetApplicationDirectory";
    function Change_Directory(dir: chars_ptr) return C_Bool
        with
            Import => True,
            Convention => C,
            External_Name => "ChangeDirectory";
end Raylib;
