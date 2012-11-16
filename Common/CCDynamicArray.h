#ifndef CCDYNAMICARRAY_H
#define CCDYNAMICARRAY_H

#include "../Common/cscomp.h"
#include <stddef.h>

#define ARRAY_MANAGED_TYPE_FLAG    0x80000000
#define CC_DYNAMIC_ARRAY_TYPE_NAME "CCDynamicArray"

struct CCDynArrayMetadata {
	size_t elementCount;
	size_t elementSize;
	int isManaged;
};

struct CCDynamicArray : ICCDynamicObject {
	
	struct CCDynArrayMetadata md;
	void* data;

	// return the type name of the object
	virtual const char *GetType();

	virtual int Dispose(const char *address, bool force);

	// serialize the object into BUFFER (which is BUFSIZE bytes)
	// return number of bytes used
	virtual size_t Serialize(const char *address, char *buffer, size_t bufsize);

	virtual void Unserialize(ptrdiff_t index, const char *serializedData, ptrdiff_t dataSize);

	long Create(size_t numElements, size_t elementSize, bool isManagedType);
	
};

#endif
