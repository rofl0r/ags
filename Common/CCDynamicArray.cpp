// *** IMPL FOR DYNAMIC ARRAYS **

#include "CCDynamicArray.h"
#include <stdlib.h>

const char *CCDynamicArray::GetType() {
	return CC_DYNAMIC_ARRAY_TYPE_NAME;
}

int CCDynamicArray::Dispose(const char *address, bool force) {
	address -= 8;

	// If it's an array of managed objects, release
	// their ref counts
	int *elementCount = (int*)address;
	if (elementCount[0] & ARRAY_MANAGED_TYPE_FLAG) {
		elementCount[0] &= ~ARRAY_MANAGED_TYPE_FLAG;
		for (int i = 0; i < elementCount[0]; i++) {
			if (elementCount[2 + i] != 0)
				ccReleaseObjectReference(elementCount[2 + i]);
		}
	}

	//delete (void*)address;
	return 1;
	/*
	
	if(this->md.isManaged) {
		
		
	}
	free(address);
	address -= 8;

	// If it's an array of managed objects, release
	// their ref counts
	int *elementCount = (int*)address;
	if (elementCount[0] & ARRAY_MANAGED_TYPE_FLAG) {
		elementCount[0] &= ~ARRAY_MANAGED_TYPE_FLAG;
		for (int i = 0; i < elementCount[0]; i++) {
			if (elementCount[2 + i] != 0)
				ccReleaseObjectReference(elementCount[2 + i]);
		}
	}

	//delete (void*)address;
	return 1; */
}

size_t CCDynamicArray::Serialize(const char *address, char *buffer, size_t bufsize) {
	int *sizeInBytes = &((int*)address)[-1];
	int sizeToWrite = *sizeInBytes + 8;
	if (sizeToWrite > bufsize)
		// buffer not big enough, ask for a bigger one
		return -sizeToWrite;
	memcpy(buffer, address - 8, sizeToWrite);
	return sizeToWrite;
	/*

	size_t sizeToWrite = this->md.elementCount * this->md.elementSize + sizeof(struct CCDynArrayMetadata);
	if (sizeToWrite > bufsize)
		// buffer not big enough, ask for a bigger one
		return -sizeToWrite;
	memcpy(buffer, &this->md, sizeof(struct CCDynArrayMetadata));
	memcpy(buffer + sizeof(struct CCDynArrayMetadata), address, sizeToWrite - sizeof(struct CCDynArrayMetadata));
	return sizeToWrite; */
	
}

void CCDynamicArray::Unserialize(ptrdiff_t index, const char *serializedData, ptrdiff_t dataSize) {
	char *newArray = new char[dataSize];
	memcpy(newArray, serializedData, dataSize);
	ccRegisterUnserializedObject(index, &newArray[8], this);	
	/*void *newArr = calloc(dataSize);
	if(newArr)
		memcpy(newArr, serializedData, dataSize);
	ccRegisterUnserializedObject(index, newArr, this); */
}

long CCDynamicArray::Create(size_t numElements, size_t elementSize, bool isManagedType) {
	char *newArray = new char[numElements * elementSize + 8];
	memset(newArray, 0, numElements * elementSize + 8);
	int *sizePtr = (int*)newArray;
	sizePtr[0] = numElements;
	sizePtr[1] = numElements * elementSize;
	if (isManagedType) 
	sizePtr[0] |= ARRAY_MANAGED_TYPE_FLAG;
	return ccRegisterManagedObject(&newArray[8], this);
	/*
	CCDynamicArray * nu = new(CCDynamicArray);
	nu->md.elementCount = numElements;
	nu->md.elementSize = elementSize;
	nu->md.isManaged = isManagedType == true;
	nu->data = calloc(numElements, elementSize);
	return ccRegisterManagedObject(this->data, nu); */
}

