#!/bin/sh

set -xe

x86_64-w64-mingw32-windres eepers.rc -O coff -o eepers.res
x86_64-w64-mingw32-gnatmake-win32 -mwindows -O3 -f -Wall -Wextra -gnat2012 eepers.adb -bargs -static -largs eepers.res -L./raylib/raylib-5.0_win64_mingw-w64/lib/ -l:libraylib.a -lwinmm -lgdi32 -static
