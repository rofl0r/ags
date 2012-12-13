#ifndef ACSCRIPTSTRING_H
#define ACSCRIPTSTRING_H

#include "acAGSCCDynamicObject.h"

struct ScriptString : AGSCCDynamicObject, ICCStringClass {
	char *text;

	virtual int Dispose(const char *address, bool force);
	virtual const char *GetType();
	virtual size_t Serialize(const char *address, char *buffer, size_t bufsize);
	virtual void Unserialize(ptrdiff_t index, const char *serializedData, ptrdiff_t dataSize);

	virtual void* CreateString(const char *fromText);

	ScriptString();
	ScriptString(const char *fromText);
};
const char* CreateNewScriptString(const char *fromText, bool reAllocate = true);
const char* String_Copy(const char *srcString);
const char* String_Append(const char *thisString, const char *extrabit);
const char* String_AppendChar(const char *thisString, long extraOne);
const char* String_ReplaceCharAt(const char *thisString, long index, long newChar);
const char* String_Truncate(const char *thisString, long length);
const char* String_Substring(const char *thisString, long index, long length);
long String_CompareTo(const char *thisString, const char *otherString, long caseSensitive);
long String_StartsWith(const char *thisString, const char *checkForString, long caseSensitive);
long String_EndsWith(const char *thisString, const char *checkForString, long caseSensitive);
const char* String_Replace(const char *thisString, const char *lookForText, 
			   const char *replaceWithText, long caseSensitive);
const char* String_LowerCase(const char *thisString);
const char* String_UpperCase(const char *thisString);
const char* String_Format(const char *texx, ...);
long String_Len(const char* texx);
long String_GetChars(const char *texx, long index);
long String_IsNullOrEmpty(const char *thisString);
long String_CaseComp(char* a, char* b);
long String_Comp(char* a, char* b);


#endif
