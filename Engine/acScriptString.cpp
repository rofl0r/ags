#include "acScriptString.hpp"
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

extern void quit(char *);
#define stricmp strcasecmp
#define strnicmp strncasecmp
#include <ctype.h>
char *strlwr(char *s) {
	char *p = s;
	while(*p) {
		if(isupper(*p))
			*p = tolower(*p);
		p++;
	}
	return s;
}

char *strupr(char *s) {
	char *p = s;
	while(*p) {
		if(islower(*p))
			*p = toupper(*p);
		p++;
	}
	return s;
}

extern char *get_translation(const char*);
#define STD_BUFFER_SIZE 3000

void my_sprintf(char *buffer, const char *fmt, va_list ap);

// ** SCRIPT STRING

const char *CreateNewScriptString(const char *fromText, bool reAllocate) {
	ScriptString *str;
	if (reAllocate) {
		str = new ScriptString(fromText);
	} else {
		str = new ScriptString();
		str->text = (char*)fromText;
	}

	ccRegisterManagedObject(str->text, str);

	/*long handle = ccRegisterManagedObject(str->text, str);
	char buffer[1000];
	sprintf(buffer, "String %p (handle %d) allocated: '%s'", str->text, handle, str->text);
	write_log(buffer);*/

	return str->text;
}

void* ScriptString::CreateString(const char *fromText) {
	return (void*)CreateNewScriptString(fromText);
}

int ScriptString::Dispose(const char *address, bool force) {
	return 1;
	// always dispose
	if (text) {
		/*    char buffer[1000];
		sprintf(buffer, "String %p deleted: '%s'", text, text);
		write_log(buffer);*/
		free(text);
	}
	delete this;
	return 1;
}

const char *ScriptString::GetType() {
	return "String";
}

size_t ScriptString::Serialize(const char *address, char *buffer, size_t bufsize) {
	if (text == NULL)
		text = "";
	StartSerialize(buffer);
	size_t l = strlen(text);
	SerializeLong(l);
	//SerializeInt(strlen(text));
	memcpy(serbuffer + bytesSoFar, text, l + 1);
	bytesSoFar += l + 1;
	return EndSerialize();
}

void ScriptString::Unserialize(ptrdiff_t index, const char *serializedData, ptrdiff_t dataSize) {
	StartUnserialize(serializedData, dataSize);
	long textsize = UnserializeLong();
	text = (char*)malloc(textsize + 1);
	memcpy(text, serializedData + bytesSoFar, textsize + 1);
	ccRegisterUnserializedObject(index, text, this);
}

ScriptString::ScriptString() {
	text = NULL;
}

ScriptString::ScriptString(const char *fromText) {
	text = strdup(fromText);
}

int String_IsNullOrEmpty(const char *thisString) {
	if ((thisString == NULL) || (thisString[0] == 0))
		return 1;
	return 0;
}

const char* String_Copy(const char *srcString) {
	return CreateNewScriptString(srcString);
}

const char* String_Append(const char *thisString, const char *extrabit) {
	size_t l1= strlen(thisString), l2 = strlen(extrabit);
	char *buffer = (char*)malloc(l1 + l2 + 1);
	memcpy(buffer, thisString, l1);
	memcpy(buffer + l1, extrabit, l2);
	buffer[l1+l2] = 0;
	return CreateNewScriptString(buffer, false);
}

const char* String_AppendChar(const char *thisString, char extraOne) {
	size_t l = thisString? strlen(thisString) : 0;
	char *buffer = (char*)malloc(l + 2);
	if(buffer) {
		if(l) memcpy(buffer, thisString, l);
		sprintf(buffer + l, "%c", extraOne);
	}
	return CreateNewScriptString(buffer, false);
}

const char* String_ReplaceCharAt(const char *thisString, int index, char newChar) {
	if(!thisString) return 0;
	size_t l = strlen(thisString);
	if ((index < 0) || ((unsigned) index >= l))
		quit("!String.ReplaceCharAt: index outside range of string");

	char *buffer = (char*)malloc(l + 1);
	memcpy(buffer, thisString, l+1);
	strcpy(buffer, thisString);
	buffer[index] = newChar;
	return CreateNewScriptString(buffer, false);
}

const char* String_Truncate(const char *thisString, int length) {
	if (length < 0)
		quit("!String.Truncate: invalid length");
	if(!thisString) return 0;
	size_t l = strlen(thisString);

	if ((unsigned) length >= l)
		return thisString;//CreateNewScriptString(strdup(thisString), false);

	char *buffer = (char*)malloc(length + 1);
	strncpy(buffer, thisString, length);
	buffer[length] = 0;
	return CreateNewScriptString(buffer, false);
}

const char* String_Substring(const char *thisString, int index, int length) {
	if (length < 0)
		quit("!String.Substring: invalid length");
	if(!thisString) return 0;
	size_t l = strlen(thisString);
	if ((index < 0) || ((unsigned) index > l))
	quit("!String.Substring: invalid index");
	if(index + length > l)
		length = l - index;

	char *buffer = (char*)malloc(length + 1);
	if(buffer) {
		memcpy(buffer, thisString + index, length);
		buffer[length] = 0;
	}
	return CreateNewScriptString(buffer, false);
}

static int mystreq(const char* cmp1, const char *cmp2, bool caseSensitive) {
	if (caseSensitive) 
		return strcmp (cmp1, cmp2) == 0 ? 1 : 0;
	else
		return stricmp(cmp1, cmp2) == 0 ? 1 : 0;
}

int String_CompareTo(const char *thisString, const char *otherString, bool caseSensitive) {
	const char *cmp1 = thisString ? thisString : "";
	const char *cmp2 = otherString ? otherString : "";
	if (caseSensitive) 
		return strcmp (cmp1, cmp2);
	else
		return stricmp(cmp1, cmp2);
}

int String_StartsWith(const char *thisString, const char *checkForString, bool caseSensitive) {
	const char *cmp1 = thisString ? thisString : "";
	const char *cmp2 = checkForString ? checkForString : "";
	size_t l = strlen(cmp2);
	return mystreq(cmp1, cmp2, l);
}

int String_EndsWith(const char *thisString, const char *checkForString, bool caseSensitive) {
	if (!thisString || !checkForString) return 0;
	
	ptrdiff_t checkAtOffset = strlen(thisString) - strlen(checkForString);
	if (checkAtOffset < 0) return 0;
	return mystreq(thisString + checkAtOffset, checkForString, caseSensitive);
}

const char* String_Replace(const char *thisString, const char *lookForText, 
			   const char *replaceWithText, bool caseSensitive) {
	char resultBuffer[STD_BUFFER_SIZE] = "";
	size_t i, thisStringLen = strlen(thisString), 
	       l = strlen(lookForText), r = strlen(replaceWithText),
	       outputSize = 0;
	       
	for (i = 0; i < thisStringLen; i++) {
		bool matchHere = false;
		if (caseSensitive)
			matchHere = (strncmp(thisString + i, lookForText, l) == 0);
		else
			matchHere = (strnicmp(thisString + i, lookForText, l) == 0);

		if (matchHere) {
			snprintf(resultBuffer + outputSize, sizeof(resultBuffer) - outputSize, "%s", replaceWithText);
			outputSize += r;
			i += l - 1;
		} else {
			resultBuffer[outputSize] = thisString[i];
			outputSize++;
		}
	}

	resultBuffer[outputSize] = 0;

	return CreateNewScriptString(resultBuffer, true);
}

const char* String_LowerCase(const char *thisString) {
	if(!thisString) return 0;
	char *buffer = strdup(thisString);
	strlwr(buffer);
	return CreateNewScriptString(buffer, false);
}

const char* String_UpperCase(const char *thisString) {
	if(!thisString) return 0;
	char *buffer = strdup(thisString);
	strupr(buffer);
	return CreateNewScriptString(buffer, false);
}

const char* String_Format(const char *texx, ...) {
	char displbuf[STD_BUFFER_SIZE];

	va_list ap;
	va_start(ap,texx);
	my_sprintf(displbuf, get_translation(texx), ap);
	va_end(ap);

	return CreateNewScriptString(displbuf);
}

int String_GetChars(const char *texx, int index) {
	if ((index < 0) || ((unsigned) index >= strlen(texx)))
		return 0;
	return texx[index];
}

// and finally, the abomination "my_sprintf"
// 64 bit: Not sure if this function is 64 bit ready
// Custom printf, needed because floats are pushed as 8 bytes
// WTF
// WTF
// WTF
// WTF
// WTF
// WTF
// this is the biggest crap i've ever seen
extern void quitprintf(char*texx, ...);

void my_sprintf(char *buffer, const char *fmt, va_list ap) {
	char* p = (char*) fmt;
	char* out = buffer;
	char fmtbuf[16];
	char *fmtp;
	union FOOLZ {
		unsigned long l;
		unsigned int i;
		char* c;
		float f;
	} theArg;// = va_arg(ap, union FOOLZ);
	
	while(*p) {
		if(*p == '%') {
			if(p[1] == '%') {
				*out++ = '%';
				p++;
				goto donefmt;
			}
			fmtp = fmtbuf;
			do {
				*(fmtp++) = *p;
				p++;
				switch(*p) {
					case 'f': case 'd': case 'c': case 's':
						*(fmtp++) = *p;
						*(fmtp++) = 0;
						theArg = va_arg(ap, union FOOLZ);
						switch(*p) {
							case 'f':
								sprintf(out, fmtbuf, theArg.f);
								break;
							case 'd':
								sprintf(out, fmtbuf, theArg.i);
								break;
							case 'c':
								sprintf(out, fmtbuf, theArg.i);
								break;
							case 's':
								sprintf(out, fmtbuf, theArg.c);
								break;
						}
						out += strlen(out);
						goto donefmt;
					default:
						break;
				}
				
			} while(*p);
		} else {
			*out++ = *p;
		}
		donefmt:
		p++;
	}
	*out = 0;
#if 0	
	
	int bufidx = 0;
	const char *curptr = fmt;
	const char *endptr;
	char spfbuffer[STD_BUFFER_SIZE];
	char fmtstring[100];
	int numargs = -1;

	while (1) {
		// copy across everything until the next % (or end of string)
		endptr = strchr(curptr, '%');
		if (endptr == NULL)
		endptr = &curptr[strlen(curptr)];
		while (curptr < endptr) {
			buffer[bufidx] = *curptr;
			curptr++;
			bufidx++;
		}
		// at this point, curptr and endptr should be equal and pointing
		// to the % or \0
		if (*curptr == 0)
			break;
		if (curptr[1] == '%') {
			// "%%", so just write a % to the output
			buffer[bufidx] = '%';
			bufidx++;
			curptr += 2;
			continue;
		}
		// find the end of the % clause
		while ((*endptr != 'd') && (*endptr != 'f') && (*endptr != 'c') &&
			(*endptr != 0) && (*endptr != 's') && (*endptr != 'x') &&
			(*endptr != 'X'))
		endptr++;

		if (numargs >= 0) {
		numargs--;
		// if there are not enough arguments, just copy the %d
		// to the output string rather than trying to format it
		if (numargs < 0)
			endptr = &curptr[strlen(curptr)];
		}

		if (*endptr == 0) {
			// something like %p which we don't support, so just write
			// the % to the output
			buffer[bufidx] = '%';
			bufidx++;
			curptr++;
			continue;
		}
		// move endptr to 1 after the end character
		endptr++;

		// copy the %d or whatever
		strncpy(fmtstring, curptr, (endptr - curptr));
		fmtstring[endptr - curptr] = 0;
		
		union FOOLZ {
			unsigned long l;
			float f;
		} theArg = va_arg(ap, union FOOLZ);

		// use sprintf to parse the actual %02d type thing
		if (endptr[-1] == 'f') {
			// floats are pushed as 8-bytes, so ensure that it knows this is a float
			float floatArg = theArg.f;
			sprintf(spfbuffer, fmtstring, floatArg);
		}
		else if ((theArg.l == (size_t) buffer) && (endptr[-1] == 's'))
			quit("Cannot use destination as argument to StrFormat");
		else if ((theArg.l < 0x10000) && (endptr[-1] == 's'))
			quit("!One of the string arguments supplied was not a string");
		else if (endptr[-1] == 's') {
			strncpy(spfbuffer, (const char*)theArg.l, STD_BUFFER_SIZE);
			spfbuffer[STD_BUFFER_SIZE - 1] = 0;
		}
		else 
			sprintf(spfbuffer, fmtstring, theArg.l);

		// use the formatted text
		buffer[bufidx] = 0;

		if (bufidx + strlen(spfbuffer) >= STD_BUFFER_SIZE)
			quitprintf("!String.Format: buffer overrun: maximum formatted string length %d chars, this string: %d chars", STD_BUFFER_SIZE, bufidx + strlen(spfbuffer));

		strcat(buffer, spfbuffer);
		bufidx += strlen(spfbuffer);
		curptr = endptr;
	}
	buffer[bufidx] = 0;
#endif
}

