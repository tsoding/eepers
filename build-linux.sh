#!/bin/sh

set -xe

gnatmake -f -O3 -Wall -Wextra -gnat2012 eepers.adb -pthread -bargs -static -largs -L./raylib/raylib-5.0_linux_amd64/lib/ -l:libraylib.a -lm
./eepers

# gnatmake -f -Wall -Wextra -gnat2022 test.adb -largs -L./raylib/raylib-5.0_linux_amd64/lib/ -l:libraylib.a -lm
# ./test
