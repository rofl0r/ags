#include "acAGSCCDynamicObject.h"
#include <string.h>
extern void quit(char *);
// *** The script serialization routines for built-in types

char* AGSCCDynamicObject::CurrentBuf(void) {
	return serbuffer + bytesSoFar;
}

int AGSCCDynamicObject::Dispose(const char *address, bool force) {
	// cannot be removed from memory
	return 0;
}

void AGSCCDynamicObject::StartSerialize(char *sbuffer) {
	bytesSoFar = 0;
	serbuffer = sbuffer;
}

void AGSCCDynamicObject::SerializeBuf(void* addr, size_t len) {
	void *iptr = (void*) CurrentBuf();
	memcpy(iptr, addr, len);
	bytesSoFar += len;
}

void AGSCCDynamicObject::SerializeLong(long val) {
	SerializeBuf(&val, sizeof(long));
}

void AGSCCDynamicObject::SerializeInt(int val) {
	SerializeBuf(&val, sizeof(int));
}

size_t AGSCCDynamicObject::EndSerialize() {
	return bytesSoFar;
}

void AGSCCDynamicObject::StartUnserialize(const char *sbuffer, int pTotalBytes) {
	bytesSoFar = 0;
	totalBytes = pTotalBytes;
	serbuffer = (char*)sbuffer;
}

int AGSCCDynamicObject::UnserializeInt() {
	if (bytesSoFar >= totalBytes)
		quit("Unserialise: internal error: read past EOF");
	int *iptr = (int*) CurrentBuf();
	bytesSoFar += sizeof(int);
	return *iptr;
}

long AGSCCDynamicObject::UnserializeLong() {
	if (bytesSoFar >= totalBytes)
		quit("Unserialise: internal error: read past EOF");
	long *iptr = (long*) CurrentBuf();
	bytesSoFar += sizeof(int);
	return *iptr;
}
