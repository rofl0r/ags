
BASE = acaudio.o acchars.o acdebug.o acdialog.o acfonts.o acgfx.o acgui.o acplatfm.o \
       acsound.o bigend.o misc.o routefnd.o scrptrt.o ac.o alogg.o almp3.o ali3dsw.o \
       acScriptString.o acAGSCCDynamicObject.o eip.o

COMMON = ../Common/csrun.o ../Common/Clib32.o ../Common/mousew32.o ../Common/sprcache.o \
	 ../Common/cscommon.o ../Common/compress.o ../Common/lzw.o \
	 ../Common/ByteArray.o ../Common/CCDynamicArray.o ../Common/acfloat.o

PLUGINS = ../Plugins/AGSflashlight/agsflashlight.o ../Plugins/agsblend/AGSBlend.o \
	  ../Plugins/ags_snowrain/ags_snowrain.o

LIBULZ = ../../cdev/cdev/debuglib/process_maps.o ../../cdev/cdev/lib/src/sblist/sblist.o

LINUX_SPECIFIC = acpllnx.o 

HQ2X3X = libsrc/hq2x/hq2x3x.o

ALFONT = libsrc/alfont-2.0.9/alfont.o

ALMP3 = libsrc/almp3-2.0.5/decoder/common.o libsrc/almp3-2.0.5/decoder/dct64_i386.o \
	libsrc/almp3-2.0.5/decoder/decode_i386.o libsrc/almp3-2.0.5/decoder/interface.o \
	libsrc/almp3-2.0.5/decoder/layer2.o libsrc/almp3-2.0.5/decoder/layer3.o \
	libsrc/almp3-2.0.5/decoder/tabinit.o

APEG = libsrc/apeg-1.2.1/display.o libsrc/apeg-1.2.1/getbits.o libsrc/apeg-1.2.1/getblk.o \
       libsrc/apeg-1.2.1/gethdr.o libsrc/apeg-1.2.1/getpic.o libsrc/apeg-1.2.1/idct.o \
       libsrc/apeg-1.2.1/motion.o libsrc/apeg-1.2.1/mpeg1dec.o libsrc/apeg-1.2.1/ogg.o \
       libsrc/apeg-1.2.1/recon.o libsrc/apeg-1.2.1/audio/apegcommon.o \
       libsrc/apeg-1.2.1/audio/audio.o libsrc/apeg-1.2.1/audio/dct64.o \
       libsrc/apeg-1.2.1/audio/decode_1to1.o libsrc/apeg-1.2.1/audio/decode_2to1.o \
       libsrc/apeg-1.2.1/audio/decode_4to1.o libsrc/apeg-1.2.1/audio/layer1.o \
       libsrc/apeg-1.2.1/audio/layer2.o libsrc/apeg-1.2.1/audio/layer3.o \
       libsrc/apeg-1.2.1/audio/mpg123.o libsrc/apeg-1.2.1/audio/readers.o \
       libsrc/apeg-1.2.1/audio/tabinit.o libsrc/apeg-1.2.1/audio/vbrhead.o

AASTR = libsrc/aastr-0.1.1/AAROT.o libsrc/aastr-0.1.1/aastr.o libsrc/aastr-0.1.1/aautil.o

CDA = libsrc/libcda-0.5/linux.o

OBJS = $(ALFONT) $(ALMP3) $(APEG) $(AASTR) $(HQ2X3X) $(CDA) $(PLUGINS) $(BASE) $(COMMON) \
       $(LINUX_SPECIFIC) $(LIBULZ)

INCDIR = ../Engine ../Common ../Common/libinclude
LIBDIR =
CFLAGS = -O2 -g -fsigned-char -Wfatal-errors 
CFLAGS += -DALLEGRO_STATICLINK -DTHIS_IS_THE_ENGINE -DLINUX_VERSION -DDISABLE_MPEG_AUDIO -DBUILTIN_PLUGINS -DRTLD_NEXT
CFLAGS += $(shell pkg-config --cflags freetype2)

CXXFLAGS = -fno-rtti -Wno-write-strings
ASFLAGS =
LIBS = -rdynamic $(shell allegro-config --libs) -laldmb -ldumb -Wl,-Bdynamic -ltheora -logg 
ifeq ($(USE_TREMOR), 1)
  LIBS += -lvorbisidec
  CFLAGS += -DUSE_TREMOR
else
  LIBS += -lvorbis
endif
LIBS += -lvorbisfile -lfreetype -logg -ldl -lpthread -lm -lc -lstdc++

PREFIX = /usr/local
CC = gcc
CXX = g++
CFLAGS   := $(addprefix -I,$(INCDIR)) $(CFLAGS)
CXXFLAGS := $(CFLAGS) $(CXXFLAGS)
ASFLAGS  := $(CFLAGS) $(ASFLAGS)
LDFLAGS  := -Wl,--as-needed $(addprefix -L,$(LIBDIR))

-include config.mak

all: ags

ags: $(OBJS)
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS) $(LIBS)

%.o: %.m
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.mm
	$(CXX) $(CXXFLAGS) -c -o $@ $<

.PHONY: clean install uninstall

clean:
	rm -f ags $(OBJS)

install: ags
	mkdir -p $(PREFIX)/bin
	cp -t $(PREFIX)/bin ags

uninstall:
	rm -f $(PREFIX)/bin/ags
