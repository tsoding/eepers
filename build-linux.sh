#!/bin/sh

set -xe

gnatmake -f -O3 -Wall -Wextra -gnat2012 -o eepers-linux eepers.adb -bargs -static -largs -L./raylib/raylib-5.0_linux_amd64/lib/ -l:libraylib.a -lm  -pthread
./eepers-linux

# gnatmake -f -Wall -Wextra -gnat2022 test.adb -largs -L./raylib/raylib-5.0_linux_amd64/lib/ -l:libraylib.a -lm
# ./test
