with Ada.Text_IO;
with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Raylib; use Raylib;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Ada.Unchecked_Conversion;
with Raylib; use Raylib;
with Raymath; use Raymath;

function Test return Integer is
    type Palette is (
      COLOR_BACKGROUND,
      COLOR_FLOOR,
      COLOR_WALL,
      COLOR_BARRICADE,
      COLOR_PLAYER,
      COLOR_DOOR_KEY,
      COLOR_BOMB,
      COLOR_LABEL,
      COLOR_GUARD,
      COLOR_URMOM,
      COLOR_GNOME,
      COLOR_CHECKPOINT,
      COLOR_EXPLOSION,
      COLOR_HEALTHBAR,
      COLOR_NEW_GAME,
      COLOR_EYES,
      COLOR_FINAL);
begin
    for C in Palette loop
        Put_Line(To_Unbounded_String(C'Image));
    end loop;
    return 0;
end;
