#!/bin/sh

set -xe

x86_64-w64-mingw32-gnatmake-win32 -mwindows -O3 -f -Wall -Wextra -gnat2022 game.adb -bargs -static -largs -L./raylib/raylib-5.0_win64_mingw-w64/lib/ -l:libraylib.a -lwinmm -lgdi32 -static
