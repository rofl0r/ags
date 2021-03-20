LOCAL_PATH := $(call my-dir)/../../../Engine

ADDITIONAL_LIBRARY_PATH := $(call my-dir)/../../nativelibs/$(TARGET_ARCH_ABI)
AGS_COMMON_PATH := $(call my-dir)/../../../Common
AGS_ENGINE_PATH := $(call my-dir)/../../../Engine
AGS_DEFAULT_CXXFLAGS = -std=c++11 -Wno-write-strings -Wno-reserved-user-defined-literal

include $(CLEAR_VARS)

BASE_PLATFORM = \
platform/android/acpland.cpp \
platform/util/libc.c

include ../../Engine/Makefile-objs
include ../../Engine/Makefile-defs.freetype

LOCAL_MODULE    := agsengine
LOCAL_SRC_FILES := $(BASE) $(BASE_PLATFORM) $(COMMON) $(COMMON_PLATFORM) $(FREETYPE) $(ALFONT) $(ALMP3) $(ALOGG) $(APEG) $(AASTR) $(AL_MIDI_PATCH)
LOCAL_CFLAGS    := -g -ffast-math -fsigned-char -Wall -Wfatal-errors -Wno-deprecated-declarations -Wno-psabi -Wno-logical-op-parentheses -Wno-bitwise-op-parentheses -Wno-logical-not-parentheses -Wno-unknown-pragmas -Wno-unknown-warning-option -DAGS_INVERTED_COLOR_ORDER -DALLEGRO_STATICLINK -DANDROID_VERSION -DDISABLE_MPEG_AUDIO -DUSE_TREMOR -I$(ADDITIONAL_LIBRARY_PATH)/include -I$(AGS_ENGINE_PATH) -I$(AGS_COMMON_PATH) -I$(AGS_COMMON_PATH)/libinclude -I$(AGS_COMMON_PATH)/libsrc/alfont-2.0.9
# TODO: find a way to reuse FT_CFLAGS from Makefile-defs.freetype
LOCAL_CFLAGS    += -I$(AGS_COMMON_PATH)/libsrc/freetype-2.1.3/include
LOCAL_CXXFLAGS  := $(LOCAL_CFLAGS) $(AGS_DEFAULT_CXXFLAGS)
LOCAL_LDLIBS    := -Wl,-Bstatic
LOCAL_LDLIBS    += $(FT_LDFLAGS)
LOCAL_LDLIBS    += -lalleg -lvorbisidec -ltheora -logg -laldmb -ldumb -lstdc++ -Wl,-Bdynamic -lc -ldl -lm -lz -llog -lGLESv1_CM -lGLESv2
LOCAL_LDFLAGS   := -Wl,-L$(ADDITIONAL_LIBRARY_PATH)/lib,--allow-multiple-definition
LOCAL_ARM_MODE  := arm

include $(BUILD_SHARED_LIBRARY)


# PE file format helper
include $(CLEAR_VARS)

LOCAL_MODULE    := pe
LOCAL_SRC_FILES := ../Android/library/jni/pe_jni.c ../Common/util/stdio_compat.c ../Engine/platform/util/pe.c
LOCAL_CFLAGS    := -O2 -g -ffast-math -fsigned-char -Wall -Wfatal-errors -I$(AGS_COMMON_PATH)
LOCAL_CXXFLAGS  := $(LOCAL_CFLAGS) -Wno-write-strings
LOCAL_LDLIBS    := -lc -llog
LOCAL_LDFLAGS   :=

include $(BUILD_SHARED_LIBRARY)


# Snowrain plugin
include $(CLEAR_VARS)

LOCAL_MODULE    := ags_snowrain
LOCAL_SRC_FILES := ../Plugins/ags_snowrain/ags_snowrain.cpp
LOCAL_CFLAGS    := -O2 -g -ffast-math -fsigned-char -Wall -Wfatal-errors -DLINUX_VERSION -DANDROID_VERSION -I$(AGS_COMMON_PATH)
LOCAL_CXXFLAGS  := $(LOCAL_CFLAGS) $(AGS_DEFAULT_CXXFLAGS)
LOCAL_LDLIBS    := -Wl,-Bstatic -lstdc++ -Wl,-Bdynamic -lc -lm -llog
LOCAL_LDFLAGS   :=

include $(BUILD_SHARED_LIBRARY)


# AGSBlend plugin
include $(CLEAR_VARS)

LOCAL_MODULE    := agsblend
LOCAL_SRC_FILES := ../Plugins/agsblend/AGSBlend.cpp
LOCAL_CFLAGS    := -O2 -g -ffast-math -fsigned-char -Wall -Wfatal-errors -DLINUX_VERSION -DANDROID_VERSION -I$(AGS_COMMON_PATH)
LOCAL_CXXFLAGS  := $(LOCAL_CFLAGS) $(AGS_DEFAULT_CXXFLAGS)
LOCAL_LDLIBS    := -Wl,-Bstatic -lstdc++ -Wl,-Bdynamic -lc -lm -llog
LOCAL_LDFLAGS   :=

include $(BUILD_SHARED_LIBRARY)


# AGSflashlight plugin
include $(CLEAR_VARS)

LOCAL_MODULE    := agsflashlight
LOCAL_SRC_FILES := ../Plugins/AGSflashlight/agsflashlight.cpp
LOCAL_CFLAGS    := -O2 -g -ffast-math -fsigned-char -Wall -Wfatal-errors -DLINUX_VERSION -DANDROID_VERSION -I$(AGS_COMMON_PATH)
LOCAL_CXXFLAGS  := $(LOCAL_CFLAGS) $(AGS_DEFAULT_CXXFLAGS)
LOCAL_LDLIBS    := -Wl,-Bstatic -lstdc++ -Wl,-Bdynamic -lc -lm -llog
LOCAL_LDFLAGS   :=

include $(BUILD_SHARED_LIBRARY)


# AGSlua plugin
include $(CLEAR_VARS)

LOCAL_MODULE    := agslua
LOCAL_SRC_FILES := ../Plugins/agslua/agslua/agslua/agslua_autogen.cpp \
../Plugins/agslua/agslua/agslua/AGSLua_Main.cpp \
../Plugins/agslua/agslua/agslua/AGSManagedObjects.cpp \
../Plugins/agslua/agslua/agslua/CompressedLuaChunks.cpp \
../Plugins/agslua/agslua/agslua/Internal.cpp \
../Plugins/agslua/agslua/agslua/LuaCustomisation.cpp \
../Plugins/agslua/agslua/agslua/LuaFromAGS.cpp \
../Plugins/agslua/agslua/agslua/LuaValueList.cpp \
../Plugins/agslua/agslua/agslua/SerializeLuaUniverse.cpp \
../Plugins/agslua/agslua/agslua/AGSStructFields.cpp \
../Plugins/agslua/agslua/agslua/DLLStuff.cpp \
../Plugins/agslua/agslua/agslua/pluto.c \
../Plugins/agslua/agslua/agslua/pdep.c \
../Plugins/agslua/agslua/agslua/lzio.c
LOCAL_CFLAGS    := -O2 -g -ffast-math -fsigned-char -Wall -Wfatal-errors -DTHIS_IS_THE_PLUGIN -DLINUX_VERSION -DANDROID_VERSION -I$(AGS_COMMON_PATH) -I$(AGS_COMMON_PATH)/../Plugins/agslua/agslua/lualibhelp/include -I$(ADDITIONAL_LIBRARY_PATH)/include
LOCAL_CXXFLAGS  := $(LOCAL_CFLAGS) $(AGS_DEFAULT_CXXFLAGS)
LOCAL_LDLIBS    := -Wl,-Bstatic -lstdc++ -llua -Wl,-Bdynamic -lc -lm -llog -lz
LOCAL_LDFLAGS   := -Wl,-L$(ADDITIONAL_LIBRARY_PATH)/lib

include $(BUILD_SHARED_LIBRARY)


# AGSSpriteFont plugin
include $(CLEAR_VARS)

LOCAL_MODULE    := agsspritefont
LOCAL_SRC_FILES := ../Plugins/AGSSpriteFont/AGSSpriteFont/AGSSpriteFont.cpp \
../Plugins/AGSSpriteFont/AGSSpriteFont/CharacterEntry.cpp \
../Plugins/AGSSpriteFont/AGSSpriteFont/color.cpp \
../Plugins/AGSSpriteFont/AGSSpriteFont/SpriteFont.cpp \
../Plugins/AGSSpriteFont/AGSSpriteFont/SpriteFontRenderer.cpp \
../Plugins/AGSSpriteFont/AGSSpriteFont/VariableWidthFont.cpp \
../Plugins/AGSSpriteFont/AGSSpriteFont/VariableWidthSpriteFont.cpp
LOCAL_CFLAGS    := -O2 -g -ffast-math -fsigned-char -Wall -Wfatal-errors -DTHIS_IS_THE_PLUGIN -DLINUX_VERSION -DANDROID_VERSION -I$(AGS_COMMON_PATH) -I$(ADDITIONAL_LIBRARY_PATH)/include
LOCAL_CXXFLAGS  := $(LOCAL_CFLAGS) $(AGS_DEFAULT_CXXFLAGS)
LOCAL_LDLIBS    := -Wl,-Bstatic -lstdc++ -Wl,-Bdynamic -lc -lm -llog -lz
LOCAL_LDFLAGS   := -Wl,-L$(ADDITIONAL_LIBRARY_PATH)/lib,--allow-multiple-definition

include $(BUILD_SHARED_LIBRARY)


# Parallax plugin
include $(CLEAR_VARS)

LOCAL_MODULE    := ags_parallax
LOCAL_SRC_FILES := ../Plugins/ags_parallax/ags_parallax.cpp
LOCAL_CFLAGS    := -O2 -g -ffast-math -fsigned-char -Wall -Wfatal-errors -DLINUX_VERSION -DANDROID_VERSION -I$(AGS_COMMON_PATH)
LOCAL_CXXFLAGS  := $(LOCAL_CFLAGS) $(AGS_DEFAULT_CXXFLAGS)
LOCAL_LDLIBS    := -Wl,-Bstatic -lstdc++ -Wl,-Bdynamic -lc -lm
LOCAL_LDFLAGS   :=

include $(BUILD_SHARED_LIBRARY)


# agspalrender (or palgorithms) plugin
include $(CLEAR_VARS)

LOCAL_MODULE    := agspalrender
LOCAL_SRC_FILES := ../Plugins/agspalrender/ags_palrender.cpp ../Plugins/agspalrender/raycast.cpp
LOCAL_CFLAGS    := -O2 -g -ffast-math -fsigned-char -Wall -Wfatal-errors -DLINUX_VERSION -DANDROID_VERSION -I$(AGS_COMMON_PATH)
LOCAL_CXXFLAGS  := $(LOCAL_CFLAGS) $(AGS_DEFAULT_CXXFLAGS)
LOCAL_LDLIBS    := -Wl,-Bstatic -lstdc++ -Wl,-Bdynamic -lc -lm
LOCAL_LDFLAGS   :=

include $(BUILD_SHARED_LIBRARY)
