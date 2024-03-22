#!/bin/sh

set -xe

gnatmake -O3 -f -Wall -Wextra -gnat2022 game.adb -bargs -static -largs -L./raylib/raylib-5.0_linux_amd64/lib/ -l:libraylib.a -lm
./game

#gnatmake -f -Wall -Wextra -gnat2022 test.adb -largs -L./raylib/raylib-5.0_linux_amd64/lib/ -l:libraylib.a -lm
#./test
