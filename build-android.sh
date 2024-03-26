#!/bin/sh

set -xe

ABIS="arm64-v8a"
API=33

BUILD_TOOLS=$ANDROID_HOME/Sdk/build-tools/34.0.0
TOOLCHAIN=$CLANG_R475365B_CUSTOM
ANDROID_NDK=$ANDROID_HOME/Sdk/ndk/26.1.10909125
NATIVE_APP_GLUE=$ANDROID_NDK/sources/android/native_app_glue
LLVM_GNAT_BIN=$LLVM_GNAT/llvm-interface/bin

SYSROOTS=$CLANG_R475365B_CUSTOM/../../../sysroots

FLAGS="-ffunction-sections -funwind-tables -fstack-protector-strong -fPIC -Wall \
	-Wformat -Werror=format-security -no-canonical-prefixes \
	-DANDROID -DPLATFORM_ANDROID -D__ANDROID_API__=$API"

mkdir -p android/res/drawable-hdpi
mkdir -p android/lib/arm64-v8a
mkdir -p android/build

SRC="eepers.adb raylib.adb raymath.ads"
OBJ="raylib raymath eepers"

for ABI in $ABIS; do
	case "$ABI" in
		"armeabi-v7a")
			CCTYPE="armv7a-linux-androideabi"
			ARCH="arm"
			ARCH_SYSROOT="arm"
			LIBPATH="arm-linux-androideabi"
			ABI_FLAGS="-std=c99 -march=armv7-a -mfloat-abi=softfp -mfpu=vfpv3-d16"
			;;

		"arm64-v8a")
			CCTYPE="aarch64-linux-android"
			ARCH="aarch64"
			ARCH_SYSROOT="arm64"
			LIBPATH="aarch64-linux-android"
			ABI_FLAGS="-std=c99 -target aarch64"
			;;
	esac
	CC="$TOOLCHAIN/bin/clang"
	SYSROOT=$SYSROOTS/ndk/$ARCH_SYSROOT
	LLD=$CLANG_R475365B_CUSTOM/bin/ld.lld
	INCLUDES="-I$NATIVE_APP_GLUE -I$SYSROOT/usr/include/$CCTYPE -I$SYSROOT/usr/include"

	# Compile native app glue
	#$CC $INCLUDES --target=$CCTYPE$API -c $NATIVE_APP_GLUE/android_native_app_glue.c -o android/build/native_app_glue.o $FLAGS $ABI_FLAGS
    $CC $INCLUDES --target=$CCTYPE$API -c android/main.c -o android/build/main.o $FLAGS $ABI_FLAGS

	# Compile project

	for file in $SRC; do
	   $LLVM_GNAT_BIN/llvm-gcc -g --target=aarch64-linux-android$API -fPIC -gnat2022 $file -c
	done

	$LLVM_GNAT/llvm-interface/bin/llvm-gnatbind -Leepers eepers.ali
    $LLVM_GNAT/llvm-interface/bin/llvm-gnatlink eepers.ali -o android/lib/$ABI/libeepers.so --LINK="$LLD" android/build/*.o -shared \
        --exclude-libs libatomic.a --build-id -z noexecstack -z relro -z now --warn-shared-textrel -u ANativeActivity_onCreate \
        --sysroot=$SYSROOT -L$CLANG_R475365B_CUSTOM/lib/clang/16.0.2/lib/linux -L$CLANG_R475365B_CUSTOM/lib/clang/16.0.2/lib/linux/$ARCH \
        -L$SYSROOT/usr/lib/$LIBPATH/$API -L$SYSROOT/usr/lib/$LIBPATH -Landroid/build -Lraylib/raylib-5.0_android_$ABI \
        -lraylib -llog -landroid -lEGL -lGLESv2 -lOpenSLES -lc -lm -ldl -l:libclang_rt.builtins-$ARCH-android.a -lunwind
done

# ______________________________________________________________________________
#
#  Build APK
# ______________________________________________________________________________
#
$BUILD_TOOLS/aapt package -f -m \
	-S android/res -J android/build -M android/AndroidManifest.xml \
	-I $ANDROID_HOME/Sdk/platforms/android-$API/android.jar

# Add resources and assets to APK
$BUILD_TOOLS/aapt package -f \
	-M android/AndroidManifest.xml -S android/res -A assets \
	-I $ANDROID_HOME/Sdk/platforms/android-$API/android.jar -F eepers.apk

# Add libraries to APK
cd android
for ABI in $ABIS; do
	$BUILD_TOOLS/aapt add ../eepers.apk lib/$ABI/libeepers.so
done
cd ..

$BUILD_TOOLS/zipalign -f 4 eepers.apk eepers.4.apk
$BUILD_TOOLS/apksigner sign --ks ~/.android/debug.keystore --ks-pass pass:android eepers.4.apk
mv -f eepers.4.apk eepers.apk