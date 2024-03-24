#!/bin/sh

set -xe

gnatmake -f -O3 -Wall -Wextra -gnat2022 eepers.adb -bargs -static -largs -framework CoreVideo -framework IOKit -framework Cocoa -framework GLUT -framework OpenGL ./raylib/raylib-5.0_macos/lib/libraylib.a

# Bundle executable in an Application
rm -rf ./eepers.app
mkdir -p ./eepers.app/Contents
mkdir ./eepers.app/Contents/MacOS
mkdir ./eepers.app/Contents/Resources
touch ./eepers.app/Contents/Info.plist

# copy binary
cp ./eepers ./eepers.app/Contents/MacOS/

#copy assets
cp -r ./assets ./eepers.app/Contents/MacOS/

# copy app icon
cp assets/icon.ico ./eepers.app/Contents/Resources/

# fill required plist file
echo '<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleExecutable</key>
  <string>eepers</string>
  <key>CFBundleIconFile</key>
  <string>icon.ico</string>
</dict>
</plist>' > eepers.app/Contents/Info.plist