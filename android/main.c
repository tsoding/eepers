#include <string.h>
#include <stdlib.h>
#include <android/log.h>
#include <android/asset_manager.h>
#include <android_native_app_glue.h>

extern void eepersinit(void);
extern void _ada_eepers(void);
extern void eepersfinal(void);

#define LOG_TAG "Eepers"
#define ALOGD(...) __android_log_print(ANDROID_LOG_DEBUG, LOG_TAG, __VA_ARGS__)

// Dummy implementations to workaround GNAT-LLVM bugs
typedef void __sigtramphandler_t (int signo, void *siginfo, void *sigcontext);

void __gnat_sigtramp (int signo, void *siginfo, void *sigcontext, __sigtramphandler_t * handler) {
	ALOGD("%s: Signal %d, siginfo %p, sigcontext %p, handler %p", __func__, signo, siginfo, sigcontext, handler);
	exit(1);
}
unsigned char _r_debug[640*1024] = {};


extern const char *GetApplicationDirectory(void);
extern const char *GetWorkingDirectory(void);
extern void SetLoadFileDataCallback(void *);

extern struct android_app *GetAndroidApp(void);
static struct android_app *app;

const unsigned char *LoadFileDataCallback(const char *fileName, int *dataSize) {
	if (memcmp(fileName, "assets/", strlen("assets/")) != 0) {
		return 0;
	}
	fileName += strlen("assets/");

	AAsset *asset = AAssetManager_open(app->activity->assetManager, fileName, AASSET_MODE_BUFFER);
	*dataSize = AAsset_getLength(asset);
	ALOGD("%s: Opening %s (size %d)", __func__, fileName, *dataSize);
	unsigned char *data = malloc(*dataSize);
	memcpy(data, AAsset_getBuffer(asset), *dataSize);
	return data;
}

int main(void) {
	app = GetAndroidApp();
	SetLoadFileDataCallback(LoadFileDataCallback);

	ALOGD("Initializing with eepersinit()");
	eepersinit();


	ALOGD("Entering Eepers main: _ada_eepers");
	_ada_eepers();
	ALOGD("Deinitializing with eepersfinal()");
	eepersfinal();
	ALOGD("%s: Done!", __func__);
}
