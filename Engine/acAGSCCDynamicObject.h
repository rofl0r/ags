#ifndef AGCSBLAHBLA_H
#define AGCSBLAHBLA_H

#include "../Common/cscomp.h"

struct AGSCCDynamicObject : ICCDynamicObject {
public:
	// default implementation
	virtual int Dispose(const char *address, bool force);
	virtual void Unserialize(ptrdiff_t index, const char *serializedData, ptrdiff_t dataSize) = 0;
protected:
	size_t bytesSoFar;
	size_t totalBytes;
	char *serbuffer;

	void StartSerialize(char *sbuffer);
	void SerializeBuf(void* addr, size_t len);
	void SerializeInt(int val);
	void SerializeLong(long val);
	size_t EndSerialize();
	void StartUnserialize(const char *sbuffer, int pTotalBytes);
	int  UnserializeInt();
	long UnserializeLong();
	char* CurrentBuf(void);
};

#endif
