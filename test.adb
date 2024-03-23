with Text_IO; use Text_IO;
with Raylib; use Raylib;
with Interfaces.C; use Interfaces.C;
with Ada.Unchecked_Conversion;

function Test return Integer is
    Sample: Sound;
begin
    Init_Window(800, 600, To_C("Test"));
    Init_Audio_Device;
    Sample := Load_Sound(To_C("assets/footsteps/boots/1.ogg"));
    Play_Sound(Sample);
    while not Window_Should_Close loop
        Begin_Drawing;
        End_Drawing;
    end loop;
    Close_Window;
    return 0;
end;
