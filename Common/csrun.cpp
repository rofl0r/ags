/*
  C-Script run-time interpreter (c) 2001 Chris Jones

  You must DISABLE OPTIMIZATIONS AND REGISTER VARIABLES in your compiler
  when compiling this, or strange results can happen.

  There is a problem with importing functions on 16-bit compilers: the
  script system assumes that all parameters are passed as 4 bytes, which
  ints are not on 16-bit systems. Be sure to define all parameters as longs,
  or join the 21st century and switch to DJGPP or Visual C++.

  This is UNPUBLISHED PROPRIETARY SOURCE CODE;
  the contents of this file may not be disclosed to third parties,
  copied or duplicated in any form, in whole or in part, without
  prior express permission from Chris Jones.
*/
//#define DEBUG_MANAGED_OBJECTS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

#ifdef _MANAGED
// ensure this doesn't get compiled to .NET IL
#pragma unmanaged
#endif

#include "cscomp.h"
#include "misc.h"
#include "bigend.h"

#ifdef AGS_BIG_ENDIAN
#include <list>
#include <algorithm>

class Span
{
public:
  Span(char *low, unsigned int len) : mLo(low), mHi(low+len), mRC(1) {}
  ~Span() {}
  
  char *mLo;
  char *mHi;
  unsigned int mRC;
  
  bool operator < (Span const &rhs) const { return mHi <= rhs.mLo; }
  bool operator == (Span const &rhs) const { return mHi > rhs.mLo && mLo < rhs.mHi; }
  //bool operator > (Span const &rhs) const { return mLo >= rhs.mHi; }
};

class Spans
{
public:
  Spans() {}
  ~Spans() {}
  
  void AddSpan(Span const &span);
  void RemoveSpan(Span const &span);
  bool const IsInSpan(char *value) const;
  
  std::list<Span> mSpans;
};

void Spans::AddSpan(Span const &span)
{
  // lower bound is basically binary find insertion position
  std::list<Span>::iterator it = std::lower_bound(mSpans.begin(), mSpans.end(), span);
  if (*it == span)
  {
    (*it).mRC++;
  }
  else
  {
    mSpans.insert(it, span);
  }
}

void Spans::RemoveSpan(Span const &span)
{
  std::list<Span>::iterator it = std::lower_bound(mSpans.begin(), mSpans.end(), span);
  if (*it == span)
  {
    if (--((*it).mRC) <= 0)
    {
      mSpans.erase(it);
    }
  }
}

bool const Spans::IsInSpan(char *value) const
{
  Span const span = Span(value, 1);
  std::list<Span>::const_iterator it = std::lower_bound(mSpans.begin(), mSpans.end(), span);
  return (*it == span);
}

#endif // AGS_BIG_ENDIAN

#include "bigend.h"

#define INSTANCE_ID_SHIFT 24
#define INSTANCE_ID_MASK  0x00000ff
#define INSTANCE_ID_REMOVEMASK 0x00ffffff
// 256 because we use 8 bits to hold instance number
#define MAX_LOADED_INSTANCES 256

ccInstance *loadedInstances[MAX_LOADED_INSTANCES] = {NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
   NULL, NULL, NULL, NULL, NULL, NULL};

char ccRunnerCopyright[] = "ScriptExecuter32 v" SCOM_VERSIONSTR " (c) 2001 Chris Jones";
static ccInstance *current_instance;
static ICCStringClass *stringClassImpl = NULL;
static int maxWhileLoops = 0;
extern void quit(char *);
extern void write_log(char *);

void dump_instruction(unsigned long *codeptr, int cps, int spp)
{
  static int line_num = 0;

  if (codeptr[0] == SCMD_LINENUM) {
    line_num = codeptr[1];
    return;
  }

  FILE *dto = ci_fopen("script.log", "at");
  fprintf(dto, "Line %3d, IP:%8d (SP:%8d) ", line_num, cps, spp);

  int l, thisop = codeptr[0] & INSTANCE_ID_REMOVEMASK, isreg = 0, t = 0;
  char *toprint = sccmdnames[thisop];
  if (toprint[0] == '$') {
    isreg = 1;
    toprint++;
  }

  if (toprint[0] == '$') {
    isreg |= 2;
    toprint++;
  }
  fprintf(dto, "%s", toprint);

  for (l = 0; l < sccmdargs[thisop]; l++) {
    t++;
    if (l > 0)
      fprintf(dto, ",");

    if ((l == 0) && (isreg & 1))
      fprintf(dto, " %s", regnames[codeptr[t]]);
    else if ((l == 1) && (isreg & 2))
      fprintf(dto, " %s", regnames[codeptr[t]]);
    else
      // MACPORT FIX 9/6/5: changed %d to %ld
      fprintf(dto, " %ld", codeptr[t]);
  }
  fprintf(dto, "\n");
  fclose(dto);
}

struct CompareStringsPartial : ICompareStrings {
  virtual int compare(const char *left, const char *right) {
    return strncmp(left, right, strlen(left));
  }
};
CompareStringsPartial ccCompareStringsPartial;

#include "../Common/CCDynamicArray.h"
#include "../Engine/acScriptString.hpp"

CCDynamicArray globalDynamicArray;

// *** MAIN CLASS CODE STARTS **

#define OBJECT_CACHE_MAGIC_NUMBER 0xa30b
#define SERIALIZE_BUFFER_SIZE 10240
const int ARRAY_INCREMENT_SIZE = 100;
const int GARBAGE_COLLECTION_INTERVAL = 100;

struct ManagedObjectPool {
	struct ManagedObject {
		long handle;
		const char *addr;
		ICCDynamicObject * callback;
		ptrdiff_t refCount;

		void init(long theHandle, const char *theAddress, ICCDynamicObject *theCallback) {
			handle = theHandle;
			addr = theAddress;
			callback = theCallback;
			refCount = 0;

			#ifdef DEBUG_MANAGED_OBJECTS
			char bufff[200];
			sprintf(bufff,"Allocated managed object handle=%d, addr=%p, type=%s", theHandle, addr, theCallback->GetType());
			write_log(bufff);
			#endif
		}

		int remove(bool force) {

			if ((callback != NULL) && (callback->Dispose(addr, force) == 0) &&
			(force == false))
				return 0;

			#ifdef DEBUG_MANAGED_OBJECTS
			char bufff[200];
			sprintf(bufff,"Line %d Disposing managed object handle=%d, addr %p", currentline, handle, addr);
			write_log(bufff);
			#endif

			handle = 0;
			addr = 0;
			callback = NULL;
			return 1;
		}

		ptrdiff_t AddRef() {

			#ifdef DEBUG_MANAGED_OBJECTS
			char bufff[200];
			sprintf(bufff,"Line %d AddRef: handle=%d new refcount=%d", currentline, handle, refCount+1);
			write_log(bufff);
			#endif

			return ++refCount;
		}

		int CheckDispose() {
			if ((refCount < 1) && (callback != NULL)) {
				if (remove(false))
				return 1;
			}
			return 0;
		}

		int SubRef() {
			refCount--;

			#ifdef DEBUG_MANAGED_OBJECTS
			char bufff[200];
			sprintf(bufff,"Line %d SubRef: handle=%d new refcount=%d", currentline, handle, refCount);
			write_log(bufff);
			#endif

			return CheckDispose();
		}

		void SubRefNoDispose() {
			refCount--;

			#ifdef DEBUG_MANAGED_OBJECTS
			char bufff[200];
			sprintf(bufff,"Line %d SubRefNoDispose: handle=%d new refcount=%d", currentline, handle, refCount);
			write_log(bufff);
			#endif
		}
	};
private:

	ManagedObject *objects;
	size_t arrayAllocLimit;
	size_t numObjects;  // not actually numObjects, but the highest index used
	size_t objectCreationCounter;  // used to do garbage collection every so often

public:

	long AddRef(long handle) {
		return objects[handle].AddRef();
	}

	int CheckDispose(long handle) {
		return objects[handle].CheckDispose();
	}

	long SubRef(long handle) {
		if ((disableDisposeForObject != NULL) && 
		    (objects[handle].addr == disableDisposeForObject))
			objects[handle].SubRefNoDispose();
		else
			objects[handle].SubRef();
		return objects[handle].refCount;
	}

	long AddressToHandle(const char *addr) {
		// this function is only called when a pointer is set
		// SLOW LOOP ALERT, improve at some point
		for (size_t kk = numObjects - 1; kk >= 1; kk--) {
			if (objects[kk].addr == addr)
				return objects[kk].handle;
		}
		return 0;
	}

#ifdef AGS_64BIT
	long AddressToHandleTrunc(const char *addr) {
		unsigned long caddr = (unsigned long)addr;
		caddr &= 0xffffffff;
		long result = -1;
		for (size_t kk = numObjects - 1; kk >= 1; kk--) {
			if (((unsigned long) objects[kk].addr & 0xffffffff) == caddr)
				if(result == -1) {
					result = objects[kk].handle;
					return result;
				}
				else dprintf(2, "warning at least 2 pointers match for truncated pointer\n");
		}
		if(result != -1) return result;
		return 0;
	}
#endif	

	const char* HandleToAddress(long handle) {
		// this function is called often (whenever a pointer is used)
		if ((handle < 1) || (handle >= arrayAllocLimit))
			return NULL;
		if (objects[handle].handle == 0)
			return NULL;
		return objects[handle].addr;
	}

	int RemoveObject(const char *address) {
		long handl = AddressToHandle(address);
		if (handl == 0)
			return 0;

		objects[handl].remove(true);
		return 1;
	}

	void RunGarbageCollectionIfAppropriate() {
		if (objectCreationCounter > GARBAGE_COLLECTION_INTERVAL) {
			objectCreationCounter = 0;
			RunGarbageCollection();
		}
	}

	void RunGarbageCollection() {
		//write_log("Running garbage collection");

		for (size_t i = 1; i < numObjects; i++) {
			if ((objects[i].refCount < 1) && (objects[i].callback != NULL)) {
				objects[i].remove(false);
			}
		}
	}

	long AddObject(const char *address, ICCDynamicObject *callback, ptrdiff_t useSlot = -1) {
		if (useSlot == -1)
			useSlot = numObjects;

		objectCreationCounter++;

		if (useSlot < arrayAllocLimit) {
			// still space in the array, so use it
			objects[useSlot].init(useSlot, address, callback);
			if (useSlot == numObjects)
				numObjects++;
			return useSlot;
		} else {
			// array has been used up
			if (useSlot == numObjects) {
				// if adding new (not un-serializing) check for empty slot
				// check backwards, since newer objects don't tend to last
				// long
				for (int i = arrayAllocLimit - 1; i >= 1; i--) {
					if (objects[i].handle == 0) {
						objects[i].init(i, address, callback);
						return i;
					}
				}
			}
			// no empty slots, expand array
			while (useSlot >= arrayAllocLimit)
				arrayAllocLimit += ARRAY_INCREMENT_SIZE;

			objects = (ManagedObject*)realloc(objects, sizeof(ManagedObject) * arrayAllocLimit);
			memset(&objects[useSlot], 0, sizeof(ManagedObject) * ARRAY_INCREMENT_SIZE);
			objects[useSlot].init(useSlot, address, callback);
			if (useSlot == numObjects)
				numObjects++;
			return useSlot;
		}
	}

	void WriteToDisk(FILE *output) {
		ptrdiff_t serializeBufferSize = SERIALIZE_BUFFER_SIZE;
		char *serializeBuffer = (char*)malloc(serializeBufferSize);

		putw(OBJECT_CACHE_MAGIC_NUMBER, output);
		putw(1, output);  // version
		putw(numObjects, output);

		// use this opportunity to clean up any non-referenced pointers
		RunGarbageCollection();

		for (size_t i = 1; i < numObjects; i++) {
			if ((objects[i].handle) && (objects[i].callback != NULL)) {
				// write the type of the object
				fputstring((char*)objects[i].callback->GetType(), output);
				// now write the object data
				ptrdiff_t bytesWritten = objects[i].callback->Serialize(objects[i].addr, serializeBuffer, serializeBufferSize);
				if ((bytesWritten < 0) && ((-bytesWritten) > serializeBufferSize)) {
					// buffer not big enough, re-allocate with requested size
					serializeBufferSize = -bytesWritten;
					serializeBuffer = (char*)realloc(serializeBuffer, serializeBufferSize);
					bytesWritten = objects[i].callback->Serialize(objects[i].addr, serializeBuffer, serializeBufferSize);
				}
				putw(bytesWritten, output);
				if (bytesWritten > 0)
				fwrite(serializeBuffer, bytesWritten, 1, output);
				putw(objects[i].refCount, output);
			} else  // write empty string if we cannot serialize it
				fputc(0, output); 
		}

		free(serializeBuffer);
	}

	int ReadFromDisk(FILE *input, ICCObjectReader *reader) {
		int serializeBufferSize = SERIALIZE_BUFFER_SIZE;
		char *serializeBuffer = (char*)malloc(serializeBufferSize);
		char typeNameBuffer[200];

		if (getw(input) != OBJECT_CACHE_MAGIC_NUMBER) {
			cc_error("Data was not written by ccSeralize");
			return -1;
		}

		if (getw(input) != 1) {
			cc_error("Invalid data version");
			return -1;
		}

		int numObjs = getw(input);

		if (numObjs >= arrayAllocLimit) {
			arrayAllocLimit = numObjs + ARRAY_INCREMENT_SIZE;
			free(objects);
			objects = (ManagedObject*)calloc(sizeof(ManagedObject), arrayAllocLimit);
		}
		numObjects = numObjs;

		for (int i = 1; i < numObjs; i++) {
			fgetstring_limit(typeNameBuffer, input, 199);
			if (typeNameBuffer[0] != 0) {
				int numBytes = getw(input);
				if (numBytes > serializeBufferSize) {
					serializeBufferSize = numBytes;
					serializeBuffer = (char*)realloc(serializeBuffer, serializeBufferSize);
				}
				if (numBytes > 0)
					fread(serializeBuffer, numBytes, 1, input);

				if (strcmp(typeNameBuffer, CC_DYNAMIC_ARRAY_TYPE_NAME) == 0)
					globalDynamicArray.Unserialize(i, serializeBuffer, numBytes);
				else
					reader->Unserialize(i, typeNameBuffer, serializeBuffer, numBytes);
				objects[i].refCount = getw(input);
			}
		}

		free(serializeBuffer);
		return 0;
	}

	void reset() {
		// de-allocate all objects
		for (size_t kk = 1; kk < arrayAllocLimit; kk++) {
			if (objects[kk].handle)
				objects[kk].remove(true);
		}
		memset(objects, 0, sizeof(ManagedObject) * arrayAllocLimit);
		numObjects = 1;
	}

	ManagedObjectPool() {
		numObjects = 1;
		arrayAllocLimit = 10;
		objects = (ManagedObject*)calloc(sizeof(ManagedObject), arrayAllocLimit);
		disableDisposeForObject = NULL;
	}

	const char* disableDisposeForObject;
};

enum ImportType {
	IT_REGULAR,
	IT_VARARGS1,
	IT_VARARGS2,
	IT_VARARGS3,
	IT_VARARGS4,
	IT_VARARGS5,
	IT_VARARGS6,
	IT_VARARGS7,
	IT_DATA,
	IT_INVALID = 0xdead
};

struct ImportInfo {
	char* name;
	void* addr;
	ImportType type;
};

struct SystemImports {
private:
	struct ImportInfo** info;
	ccInstance **isScriptImp;
	size_t numimports;
	size_t bufferSize;
	ccTreeMap btree;

public:
	ssize_t add(char *, char *, ccInstance*);
	void remove(char *);
	struct ImportInfo* get_addr_of(char *);
	ssize_t get_index_of(char *);
	ccInstance* is_script_import(char *);
	void remove_range(char *, unsigned long);
	char* get_name_for_addr(char* myaddr);
	void clear() {
		for(size_t i = 0; i < numimports; i++)
			free(info[i]);
		numimports = 0;
		btree.clear();
	}
	//  void remove_all_script_exports();

	SystemImports() {
		numimports = 0;
		bufferSize = 0;
		info = 0;
		isScriptImp = NULL;
	}
};
/*
void SystemImports::remove_all_script_exports()
{
  int o;

  // Cut off at the first script exports - used to reset script system
  // Although FreeInstance removes them anyway, we might be
  // in AbortAndDestroy so we can't free it yet
  for (o = 0; o < numimports; o++) {
    if (isScriptImp[o]) {
      numimports = o;
      break;
    }
  }

}*/

#if 1
#include "../../cdev/cdev/debuglib/process_maps.h"
sblist* processmap;
static int is_in_executable_section(void* addr) {
	if(!processmap) processmap = process_maps_get(getpid());
	map_data* map = find_map_for_addr(processmap, addr);
	if(!map) {
		processmap = process_maps_get(getpid());
		map = find_map_for_addr(processmap, addr);
		if (!map) __asm__("int3");
	}
	if(map->perms & MDP_X) return 1;
	return 0;
}
#endif

enum ImportType getImportType(char* funcname, void* addr) {
	if(0) {
		return IT_REGULAR;
	} else if(!strcmp("game", funcname) || !strcmp("gs_globals", funcname) || !strcmp("mouse", funcname)
		||!strcmp("palette", funcname) ||  !strcmp("system", funcname) || !strcmp("savegameindex", funcname)
	) {
		return IT_DATA;
	} else if(!strcmp("String::Format^101", funcname)) {
		return IT_VARARGS1;
	} else if(!strcmp("Display", funcname)) {
		return IT_VARARGS1;
	} else if(!strcmp("AbortGame", funcname)) {
		return IT_VARARGS1;
	} else if(!strcmp("Character::Say^101", funcname) || !strcmp("Character::Think^101", funcname)) {
		return IT_VARARGS2;
	} else if(!strcmp("DisplaySpeech", funcname)) {
		return IT_VARARGS2;
	} else if(!strcmp("DisplayThought", funcname)) {
		return IT_VARARGS2;
	} else if(!strcmp("StrFormat", funcname)) {
		return IT_VARARGS2;
	} else if(!strcmp("RawPrint", funcname)) {
		return IT_VARARGS3;
	} else if(!strcmp("DisplayAt", funcname)) {
		return IT_VARARGS4;
	} else if(!strcmp("DisplayTopBar", funcname)) {
		return IT_VARARGS5;
	} else if(!strcmp("Overlay::SetText^104", funcname)) {
		return IT_VARARGS5;
	} else if(!strcmp("DrawingSurface::DrawString^104", funcname)) {
		return IT_VARARGS5;
	} else if(!strcmp("Overlay::CreateTextual^106", funcname)) {
		return IT_VARARGS6;
	} else if(!strcmp("CreateTextOverlay", funcname)) {
		return IT_VARARGS6;
	} else if(!strcmp("SetTextOverlay", funcname)) {
		return IT_VARARGS7;
	} else {
		/* this crap engine uses the same mechanism for function calls and data
		 * since we can't differ data references otherwise, we have to lookup
		 * the permissions of the mapping it lives in */
		if(is_in_executable_section(addr)) return IT_REGULAR;
		return IT_DATA;
	}
}

ssize_t SystemImports::add(char *namm, char *add, ccInstance *anotherscr = NULL) {
	ssize_t ixof;

	if ((ixof = get_index_of(namm)) >= 0) {
		// Only allow override if not a script-exported function
		if (anotherscr == NULL) {
			info[ixof]->addr = add;
			isScriptImp[ixof] = anotherscr;
		}
		return 0;
	}

	ixof = numimports;
	for (size_t ii = 0; ii < numimports; ii++) {
		if (info[ii]->name == NULL) {
			ixof = ii;
			break;
		}
	}

	if (ixof >= this->bufferSize) {
		if (this->bufferSize > 50000)
			return -1;  // something has gone badly wrong
		this->bufferSize += 1000;
		this->info = (struct ImportInfo**) realloc(this->info, sizeof(struct ImportInfo*) * this->bufferSize);
		this->isScriptImp = (ccInstance**)realloc(this->isScriptImp, sizeof(ccInstance*) * this->bufferSize);
	}

	btree.addEntry(namm, ixof);
	if(ixof == numimports)
		info[ixof] = (struct ImportInfo *) malloc(sizeof(struct ImportInfo));
	info[ixof]->name = namm;
	info[ixof]->addr = add;
	info[ixof]->type = getImportType(namm, add);
	isScriptImp[ixof] = anotherscr;

	if (ixof == numimports)
		numimports++;
	return 0;
}

void SystemImports::remove(char *nameToRemove) {
	ssize_t idx = get_index_of(nameToRemove);
	if (idx < 0)
		return;
	btree.removeEntry(info[idx]->name);
	info[idx]->name = 0;
	info[idx]->addr = 0;
	info[idx]->type = IT_INVALID;
	isScriptImp[idx] = 0;
	/*numimports--;
	for (int ii = idx; ii < numimports; ii++) {
	this->name[ii] = this->name[ii + 1];
	addr[ii] = addr[ii + 1];
	isScriptImp[ii] = isScriptImp[ii + 1];
	}*/
}

struct ImportInfo* SystemImports::get_addr_of(char *namw) {
	ssize_t i = get_index_of(namw);
	if (i < 0)
		return 0;

	return info[i];
}

ssize_t SystemImports::get_index_of(char *namw) {
	ssize_t bestMatch = -1;
	char altName[200];

	ssize_t idx = btree.findValue((const char*)namw);
	if (idx >= 0) return idx;
	
	sprintf(altName, "%s$", namw);

	// if it's a function with a mangled name, allow it
	idx = btree.findValue(altName, &ccCompareStringsPartial);
	if (idx >= 0) return idx;
	
	/*
	int o;
	for (o = 0; o < numimports; o++) {
	if (strcmp(name[o], namw) == 0)
	return o;
	// if it's a function with a mangled name, allow it
	if (strncmp(name[o], altName, strlen(altName)) == 0)
	return o;
	}*/

	if ((strlen(namw) > 3) && 
	((namw[strlen(namw) - 2] == '^') || (namw[strlen(namw) - 3] == '^'))) {
		// Function with number of prametrs on the end
		// attempt to find it without the param count
		strcpy(altName, namw);
		strrchr(altName, '^')[0] = 0;

		return get_index_of(altName);
	}

	return -1;
}

ccInstance* SystemImports::is_script_import(char *namw) {
	if (namw == NULL) {
	quit("is_script_import: NULL pointer passed");
	}

	ssize_t idx = get_index_of(namw);
	if (idx < 0) return 0;

	return isScriptImp[idx];
}

char* SystemImports::get_name_for_addr(char* myaddr) {
	for (size_t o = 0; o < numimports; o++)
		if (info[o]->addr == myaddr) return info[o]->name;

	return 0;
}

// Remove all symbols whose addresses are in the supplied range
void SystemImports::remove_range(char *from, unsigned long dist) {
	unsigned long startaddr = (unsigned long)from;
	for (size_t o = 0; o < numimports; o++) {
		if (!info[o]->name) continue;

		unsigned long thisaddr = (unsigned long) info[o]->addr;
		if ((thisaddr >= startaddr) && (thisaddr < startaddr + dist)) {
			btree.removeEntry(info[o]->name);
			info[o]->name = 0;
			info[o]->addr = 0;
			info[o]->type = IT_INVALID;
			isScriptImp[o] = 0;
			/*numimports--;
			for (int p = o; p < numimports; p++) {
				name[p] = name[p + 1];
				addr[p] = addr[p + 1];
				isScriptImp[p] = isScriptImp[p + 1];
			}
			o--;*/
		}
	}
}

void nullfree(void *data)
{
  if (data != NULL)
    free(data);
}

ManagedObjectPool pool;
SystemImports simp;

#ifdef AGS_BIG_ENDIAN
Spans gSpans;
#endif

char* ccGetNameForFunc(long addrof) {
	return simp.get_name_for_addr((char*)addrof);
}

void ccAddExternalSymbol(char *namof, void *addrof)
{
  simp.add(namof, (char *)addrof, NULL);
}

void ccRemoveExternalSymbol(char *namof)
{
  simp.remove(namof);
}

void ccRemoveAllSymbols()
{
  simp.clear();
}

void *ccGetSymbolAddress(char *namof)
{
	struct ImportInfo* imp = simp.get_addr_of(namof);
	return imp->type == IT_DATA ? imp->addr : imp;
}

ccInstance *ccGetCurrentInstance()
{
  return current_instance;
}

// If a while loop does this many iterations without the
// NofityScriptAlive function getting called, the script
// aborts. Set to 0 to disable.
void ccSetScriptAliveTimer (int numloop) {
  maxWhileLoops = numloop;
}

void ccNotifyScriptStillAlive () {
  if (current_instance != NULL)
    current_instance->flags |= INSTF_RUNNING;
}

ccInstance *ccCreateInstanceEx(ccScript * scri, ccInstance * joined)
{
  int i;

  currentline = -1;
  if ((scri == NULL) && (joined != NULL))
    scri = joined->instanceof;

  if (scri == NULL) {
    cc_error("null pointer passed");
    return NULL;
  }

  // allocate and copy all the memory with data, code and strings across
  ccInstance *cinst = (ccInstance *) malloc(sizeof(ccInstance));
  cinst->instanceof = NULL;
  cinst->exportaddr = NULL;
  cinst->callStackSize = 0;

  if (joined != NULL) {
    // share memory space with an existing instance (ie. this is a thread/fork)
    cinst->globaldatasize = joined->globaldatasize;
    cinst->globaldata = joined->globaldata;
  } 
  else {
    // create own memory space
    cinst->globaldatasize = scri->globaldatasize;
    if (cinst->globaldatasize > 0) {
      cinst->globaldata = (char *)malloc(cinst->globaldatasize);
      memcpy(cinst->globaldata, scri->globaldata, cinst->globaldatasize);
    }
    else
      cinst->globaldata = NULL;
  }
  cinst->codesize = scri->codesize;

  if (cinst->codesize > 0) {
    cinst->code = (unsigned long *)malloc(cinst->codesize * sizeof(long));
    memcpy(cinst->code, scri->code, cinst->codesize * sizeof(long));
  }
  else
    cinst->code = NULL;
  // just use the pointer to the strings since they don't change
  cinst->strings = scri->strings;
  cinst->stringssize = scri->stringssize;
  // create a stack
  cinst->stacksize = CC_STACK_SIZE;
  cinst->stack = (char *)calloc(1, cinst->stacksize);
  if (cinst->stack == NULL) {
    cc_error("not enough memory to allocate stack");
    return NULL;
  }

  // find a LoadedInstance slot for it
  for (i = 0; i < MAX_LOADED_INSTANCES; i++) {
    if (loadedInstances[i] == NULL) {
      loadedInstances[i] = cinst;
      cinst->loadedInstanceId = i;
      break;
    }
    if (i == MAX_LOADED_INSTANCES - 1) {
      cc_error("too many active instances");
      ccFreeInstance(cinst);
      return NULL;
    }
  }

  // set up the initial registers to zero
  memset(&cinst->registers[0], 0, sizeof(long) * CC_NUM_REGISTERS);

  // find the real address of all the imports
  long *import_addrs = (long *) calloc(scri->numimports, sizeof(long));
  if (scri->numimports == 0)
    import_addrs = NULL;

  for (i = 0; i < scri->numimports; i++) {
    // MACPORT FIX 9/6/5: changed from NULL TO 0
    if (scri->imports[i] == 0) {
      import_addrs[i] = 0;
      continue;
    }
    import_addrs[i] = (long) ccGetSymbolAddress(scri->imports[i]);
    if (import_addrs[i] == 0) {
      nullfree(import_addrs);
      cc_error("unresolved import '%s'", scri->imports[i]);
      ccFreeInstance(cinst);
      return NULL;
    }
  }

  // perform the fixups
  for (i = 0; i < scri->numfixups; i++) {
    long fixup = scri->fixups[i];
    switch (scri->fixuptypes[i]) {
    case FIXUP_GLOBALDATA:
      cinst->code[fixup] += (long)&cinst->globaldata[0];
      break;
    case FIXUP_FUNCTION:
//      cinst->code[fixup] += (long)&cinst->code[0];
      break;
    case FIXUP_STRING:
      cinst->code[fixup] += (long)&cinst->strings[0];
      break;
    case FIXUP_IMPORT: {
      unsigned long setTo = import_addrs[cinst->code[fixup]];
      ccInstance *scriptImp = simp.is_script_import(scri->imports[cinst->code[fixup]]);
      // If the call is to another script function (in a different
      // instance), replace the call with CALLAS so it doesn't do
      // a real x86 JMP to the instruction
      if (scriptImp != NULL) {
        if (cinst->code[fixup + 1] == SCMD_CALLEXT) {
          // save the instance ID in the top 4 bits of the instruction
          cinst->code[fixup + 1] = SCMD_CALLAS;
          cinst->code[fixup + 1] |= ((unsigned long)scriptImp->loadedInstanceId) << INSTANCE_ID_SHIFT;
        }
      }
      cinst->code[fixup] = setTo;
      break;
    }
    case FIXUP_DATADATA:
      if (joined == NULL)
      {
#ifdef AGS_BIG_ENDIAN
        // supposedly these are only used for strings...
        long *dataPtr = (long *)(&cinst->globaldata[fixup]);
        *dataPtr = __int_swap_endian(*dataPtr);
#endif
      long temp = 0;
      memcpy(&temp, (char*)&(cinst->globaldata[fixup]), 4);
      temp += (long)cinst->globaldata;
      memcpy(&(cinst->globaldata[fixup]), &temp, 4);
#ifdef AGS_BIG_ENDIAN
        // leave the address swapped - will be read in and flipped every time
        *dataPtr = __int_swap_endian(*dataPtr);
#endif
      }
      break;
    case FIXUP_STACK:
      cinst->code[fixup] += (long)&cinst->stack[0];
      break;
    default:
      nullfree(import_addrs);
      cc_error("internal fixup index error");
      ccFreeInstance(cinst);
      return NULL;
    }
  }
  nullfree(import_addrs);

  cinst->exportaddr = (char**)malloc(sizeof(char*) * scri->numexports);

  // find the real address of the exports
  for (i = 0; i < scri->numexports; i++) {
    long etype = (scri->export_addr[i] >> 24L) & 0x000ff;
    long eaddr = (scri->export_addr[i] & 0x00ffffff);
    if (etype == EXPORT_FUNCTION)
      cinst->exportaddr[i] = (char *)(eaddr * sizeof(long) + (long)(&cinst->code[0]));
    else if (etype == EXPORT_DATA)
      cinst->exportaddr[i] = eaddr + (&cinst->globaldata[0]);
    else {
      cc_error("internal export fixup error");
      ccFreeInstance(cinst);
      return NULL;
    }
  }
  cinst->instanceof = scri;
  cinst->pc = 0;
  cinst->flags = 0;
  if (joined != NULL)
    cinst->flags = INSTF_SHAREDATA;
  scri->instances++;

  if ((scri->instances == 1) && (ccGetOption(SCOPT_AUTOIMPORT) != 0)) {
    // import all the exported stuff from this script
    for (i = 0; i < scri->numexports; i++) {
      if (simp.add(scri->exports[i], cinst->exportaddr[i], cinst)) {
        ccFreeInstance(cinst);
        cc_error("Export table overflow at '%s'", scri->exports[i]);
        return NULL;
      }
    }
  }
  
#ifdef AGS_BIG_ENDIAN
  gSpans.AddSpan(Span((char *)cinst->globaldata, cinst->globaldatasize));
#endif
  
  return cinst;
}

// changes all pointer variables (ie. strings) to have the relative address, to allow
// the data segment to be saved to disk
void ccFlattenGlobalData(ccInstance * cinst)
{
  ccScript *scri = cinst->instanceof;
  int i;

  if (cinst->flags & INSTF_SHAREDATA)
    return;

  // perform the fixups
  for (i = 0; i < scri->numfixups; i++) {
    long fixup = scri->fixups[i];
    if (scri->fixuptypes[i] == FIXUP_DATADATA) {
#ifdef AGS_BIG_ENDIAN
      // supposedly these are only used for strings...
      long *dataPtr = (long *)(&cinst->globaldata[fixup]);
      *dataPtr = __int_swap_endian(*dataPtr);
#endif
      long temp = 0;
      memcpy(&temp, (char*)&(cinst->globaldata[fixup]), 4);
      temp -= (long)cinst->globaldata;
      memcpy(&(cinst->globaldata[fixup]), &temp, 4);
#ifdef AGS_BIG_ENDIAN
      // leave the address swapped - will be read in and flipped every time
      *dataPtr = __int_swap_endian(*dataPtr);
#endif
    }
  }

}

// restores the pointers after a save
void ccUnFlattenGlobalData(ccInstance * cinst)
{
  ccScript *scri = cinst->instanceof;
  int i;

  if (cinst->flags & INSTF_SHAREDATA)
    return;

  // perform the fixups
  for (i = 0; i < scri->numfixups; i++) {
    long fixup = scri->fixups[i];
    if (scri->fixuptypes[i] == FIXUP_DATADATA) {
#ifdef AGS_BIG_ENDIAN
      // supposedly these are only used for strings...
      long *dataPtr = (long *)(&cinst->globaldata[fixup]);
      *dataPtr = __int_swap_endian(*dataPtr);
#endif
      long temp = 0;
      memcpy(&temp, (char*)&(cinst->globaldata[fixup]), 4);
      temp += (long)cinst->globaldata;
      memcpy(&(cinst->globaldata[fixup]), &temp, 4);
#ifdef AGS_BIG_ENDIAN
      // leave the address swapped - will be read in and flipped every time
      *dataPtr = __int_swap_endian(*dataPtr);
#endif
    }
  }

}


ccInstance *ccCreateInstance(ccScript * scri)
{
  return ccCreateInstanceEx(scri, NULL);
}

ccInstance *ccForkInstance(ccInstance * instoff)
{
  return ccCreateInstanceEx(instoff->instanceof, instoff);
}

void ccFreeInstance(ccInstance * cinst)
{
  if (cinst->instanceof != NULL) {
    cinst->instanceof->instances--;
    if (cinst->instanceof->instances == 0) {
      simp.remove_range((char *)&cinst->globaldata[0], cinst->globaldatasize);
      simp.remove_range((char *)&cinst->code[0], cinst->codesize * sizeof(long));
    }
  }
  
  // remove from the Active Instances list
  if (loadedInstances[cinst->loadedInstanceId] == cinst)
    loadedInstances[cinst->loadedInstanceId] = NULL;

#ifdef AGS_BIG_ENDIAN
  gSpans.RemoveSpan(Span((char *)cinst->globaldata, cinst->globaldatasize));
#endif
  
  if ((cinst->flags & INSTF_SHAREDATA) == 0)
    nullfree(cinst->globaldata);

  nullfree(cinst->code);
  cinst->strings = NULL;
  nullfree(cinst->stack);
  nullfree(cinst->exportaddr);
  free(cinst);
}

// get a pointer to a variable or function exported by the script
char *ccGetSymbolAddr(ccInstance * inst, char *symname) {
	ptrdiff_t k;
	size_t l = strlen(symname);

	for (k = 0; k < inst->instanceof->numexports; k++) {
		size_t l2 = strlen(inst->instanceof->exports[k]);
		if ((l2 == l || l2 > l) 
		    && (memcmp(inst->instanceof->exports[k], symname, l) == 0)
		    && (l2 == l || inst->instanceof->exports[k][l] == '$') )
		return inst->exportaddr[k];
	}
	return NULL;
}

void ccSetStringClassImpl(ICCStringClass *theClass) {
  stringClassImpl = theClass;
}

long ccRegisterManagedObject_(const void *object, ICCDynamicObject *callback, char* file, int line) {
  long handl = pool.AddObject((const char*)object, callback);

#ifdef DEBUG_MANAGED_OBJECTS
  char bufff[200];
  sprintf(bufff,"%s:%d -> sourceline: %d: Register managed object type '%s' handle=%d addr=%p",
	  file, line, currentline,
	  ((callback == NULL) ? "(unknown)" : callback->GetType()), handl, object);
  write_log(bufff);
#if 0  
  if((unsigned long) object > 0xFFFFFFFF && !strcmp(callback->GetType(), "String"))
	  ccSetOption(SCOPT_DEBUGRUN, 1);
  else
	    ccSetOption(SCOPT_DEBUGRUN, 0);
#endif
#endif

  return handl;
}

long ccRegisterUnserializedObject(ptrdiff_t index, const void *object, ICCDynamicObject *callback) {
  return pool.AddObject((const char*)object, callback, index);
}

int ccUnRegisterManagedObject(const void *object) {
  return pool.RemoveObject((const char*)object);
}

void ccAttemptDisposeObject(long handle) {
  if (pool.HandleToAddress(handle) != NULL)
    pool.CheckDispose(handle);
}

void ccUnregisterAllObjects() {
  pool.reset();
}

void ccSerializeAllObjects(FILE *output) {
  pool.WriteToDisk(output);
}

int ccUnserializeAllObjects(FILE *input, ICCObjectReader *callback) {
  // un-register all existing objects, ready for the un-serialization
  ccUnregisterAllObjects();
  return pool.ReadFromDisk(input, callback);
}
long ccGetObjectHandleFromAddress_(const char *address, char* file, int line) {
  // set to null
  if (address == NULL)
    return 0;

  long handl = pool.AddressToHandle(address);

#ifdef DEBUG_MANAGED_OBJECTS
  char bufff[200];
  sprintf(bufff,"%s:%d -> script Line %d WritePtr: %p to %d", file, line, currentline, address, handl);
  write_log(bufff);
#endif

  if (handl == 0) {
#ifdef AGS_64BIT	  
	  if((unsigned long) address > 0xffffFFFF) {
		/* truncated pointer */
#ifdef DEBUG_MANAGED_OBJECTS
		dprintf(2, "XXXXXXXXXXXXXXX warning: truncated pointer\n");
#endif
		handl = pool.AddressToHandleTrunc(address);
		if(handl) return handl;
	  }
#endif
#ifdef DEBUG_MANAGED_OBJECTS
	  __asm__("int3");
#endif
    cc_error("Pointer cast failure: the object being pointed to is not in the managed object pool");
    return -1;
  }
  return handl;
}

const char *ccGetObjectAddressFromHandle(long handle) {
  if (handle == 0) {
    return NULL;
  }
  const char *addr = pool.HandleToAddress(handle);

#ifdef DEBUG_MANAGED_OBJECTS
  char bufff[200];
  sprintf(bufff,"Line %d ReadPtr: %d to %p", currentline, handle, addr);
  write_log(bufff);
#endif

  if (addr == NULL) {
    cc_error("Error retrieving pointer: invalid handle %d", handle);
    return NULL;
  }
  return addr;
}

int ccAddObjectReference(long handle) {
  if (handle == 0)
    return 0;

  return pool.AddRef(handle);
}

int ccReleaseObjectReference(long handle) {
  if (handle == 0)
    return 0;

  if (pool.HandleToAddress(handle) == NULL) {
    cc_error("Error releasing pointer: invalid handle %d", handle);
    return -1;
  }

  return pool.SubRef(handle);
}

new_line_hook_type new_line_hook = NULL;

void ccSetDebugHook(new_line_hook_type jibble)
{
  new_line_hook = jibble;
}

// Macros to maintain the call stack
#define PUSH_CALL_STACK(inst) \
  if (inst->callStackSize >= MAX_CALL_STACK) { \
    cc_error("Call stack overflow (recursive call error?)"); \
    return -1; \
  } \
  inst->callStackLineNumber[inst->callStackSize] = inst->line_number;  \
  inst->callStackCodeInst[inst->callStackSize] = inst->runningInst;  \
  inst->callStackAddr[inst->callStackSize] = inst->pc;  \
  inst->callStackSize++ 

#define POP_CALL_STACK(inst) \
  if (inst->callStackSize < 1) { \
    cc_error("Call stack underflow -- internal error"); \
    return -1; \
  } \
  inst->callStackSize--;\
  inst->line_number = inst->callStackLineNumber[inst->callStackSize];\
  currentline = inst->line_number
//long SetTextOverlay(long ovrid,long xx,long yy,long wii,long fontid,long clr,char*texx,...)
long call_variadic_function_7args(void* addr, int numparm, long *parms, int offset) {
	parms += offset;
	long (*fparam) (long, long, long, long, long, long, long, ...);
	fparam = (long (*)(long, long, long, long, long, long, long, ...))addr;
	switch (numparm) {
		case 7:
			return fparam(parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 8:
			return fparam(parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 9:
			return fparam(parms[8], parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		default:
			cc_error("too many arguments in call to function");
			return -1;		
	}
//long x, long y, long width, long font, long colour, const char* text, ...
}



long call_variadic_function_6args(void* addr, int numparm, long *parms, int offset) {
	parms += offset;
	long (*fparam) (long, long, long, long, long, long, ...);
	fparam = (long (*)(long, long, long, long, long, long, ...))addr;
	switch (numparm) {
		case 6:
			return fparam(parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 7:
			return fparam(parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 8:
			return fparam(parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 9:
			return fparam(parms[8], parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		default:
			cc_error("too many arguments in call to function");
			return -1;		
	}
//long x, long y, long width, long font, long colour, const char* text, ...
}
//long DisplayAt(long xxp,long yyp,long widd,char*texx, ...)

long call_variadic_function_4args(void* addr, int numparm, long *parms, int offset) {
	parms += offset;
	long (*fparam) (long, long, long, long, ...);
	fparam = (long (*)(long, long, long, long, ...))addr;
	switch (numparm) {
		case 4:
			return fparam(parms[3], parms[2], parms[1], parms[0]);
		case 5:
			return fparam(parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 6:
			return fparam(parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 7:
			return fparam(parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 8:
			return fparam(parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 9:
			return fparam(parms[8], parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		default:
			cc_error("too many arguments in call to function");
			return -1;		
	}
//long x, long y, long width, long font, long colour, const char* text, ...
}

long call_variadic_function_5args(void* addr, int numparm, long *parms, int offset) {
	parms += offset;
	long (*fparam) (long, long, long, long, long, ...);
	fparam = (long (*)(long, long, long, long, long, ...))addr;
	switch (numparm) {
		case 5:
			return fparam(parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 6:
			return fparam(parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 7:
			return fparam(parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 8:
			return fparam(parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 9:
			return fparam(parms[8], parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		default:
			cc_error("too many arguments in call to function");
			return -1;		
	}
//long x, long y, long width, long font, long colour, const char* text, ...
}

// parm list is backwards (last arg is parms[0])
long call_variadic_function_1arg(void* addr, int numparm, long *parms, int offset) {
	parms += offset;
	long (*fparam) (long, ...);
	fparam = (long (*)(long, ...))addr;
	
	switch(numparm) {
		case 1:
			return fparam(parms[0]);
		case 2:
			return fparam(parms[1], parms[0]);
		case 3:
			return fparam(parms[2], parms[1], parms[0]);
		case 4:
			return fparam(parms[3], parms[2], parms[1], parms[0]);
		case 5:
			return fparam(parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 6:
			return fparam(parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 7:
			return fparam(parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 8:
			return fparam(parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 9:
			return fparam(parms[8], parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		default:
			cc_error("too many arguments in call to function");
			return -1;
	}
}

// parm list is backwards (last arg is parms[0])
long call_variadic_function_3args(void* addr, int numparm, long *parms, int offset) {
	parms += offset;
	long (*fparam) (long, long, long, ...);
	fparam = (long (*)(long, long, long, ...))addr;
	
	switch(numparm) {
		case 3:
			return fparam((long)parms[2], (long)parms[1], parms[0]);
		case 4:
			return fparam((long)parms[3], (long)parms[2], parms[1], parms[0]);
		case 5:
			return fparam((long)parms[4], (long)parms[3], parms[2], parms[1], parms[0]);
		case 6:
			return fparam((long)parms[5], (long)parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 7:
			return fparam((long)parms[6], (long)parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 8:
			return fparam((long)parms[7], (long)parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 9:
			return fparam((long)parms[8], (long)parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		default:
			cc_error("too many or too few arguments in call to function");
			return 0;
	}
}

// parm list is backwards (last arg is parms[0])
//long __sc_displayspeech(long chid,char*texx, ...)
long call_variadic_function_2args(void* addr, int numparm, long *parms, int offset) {
	parms += offset;
	long (*fparam) (long, long, ...);
	fparam = (long (*)(long, long, ...))addr;
	
	switch(numparm) {
		case 2:
			return fparam(parms[1], parms[0]);
		case 3:
			return fparam(parms[2], parms[1], parms[0]);
		case 4:
			return fparam(parms[3], parms[2], parms[1], parms[0]);
		case 5:
			return fparam(parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 6:
			return fparam(parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 7:
			return fparam(parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 8:
			return fparam(parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		case 9:
			return fparam(parms[8], parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		default:
			cc_error("too many or few arguments in call to function");
			return 0;
	}
}

// parm list is backwards (last arg is parms[0])
long call_function(void* addr, int numparm, long *parms, int offset) {
	parms += offset;
	
	switch(numparm) {
		case 0: {
			long (*fparam) (void);
			fparam = (long (*)(void))addr;
			return fparam();
		}
		case 1: {
			long (*fparam) (long);
			fparam = (long (*)(long))addr;
			return fparam(parms[0]);
		}
		case 2: {
			long (*fparam) (long, long);
			fparam = (long (*)(long, long))addr;
			return fparam(parms[1], parms[0]);
		}
		case 3: {
			long (*fparam) (long, long, long);
			fparam = (long (*)(long, long, long))addr;
			return fparam(parms[2], parms[1], parms[0]);
		}
		case 4: {
			long (*fparam) (long, long, long, long);
			fparam = (long (*)(long, long, long, long))addr;
			return fparam(parms[3], parms[2], parms[1], parms[0]);
		}
		case 5: {
			long (*fparam) (long, long, long, long, long);
			fparam = (long (*)(long, long, long, long, long))addr;
			return fparam(parms[4], parms[3], parms[2], parms[1], parms[0]);
		}
		case 6: {
			long (*fparam) (long, long, long, long, long, long);
			fparam = (long (*)(long, long, long, long, long, long))addr;
			return fparam(parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		}
		case 7: {
			long (*fparam) (long, long, long, long, long, long, long);
			fparam = (long (*)(long, long, long, long, long, long, long))addr;
			return fparam(parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		}
		case 8: {
			long (*fparam) (long, long, long, long, long, long, long, long);
			fparam = (long (*)(long, long, long, long, long, long, long, long))addr;
			return fparam(parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		}
		case 9: {
			long (*fparam) (long, long, long, long, long, long, long, long, long);
			fparam = (long (*)(long, long, long, long, long, long, long, long, long))addr;
			return fparam(parms[8], parms[7], parms[6], parms[5], parms[4], parms[3], parms[2], parms[1], parms[0]);
		}
		default:
			cc_error("too many arguments in call to function");
			return -1;
	}
}

long do_function_call(struct ImportInfo *imp, int num_args_to_func, long *callstack, int callstackcount) {
	switch(imp->type) {
		case IT_REGULAR:
			return call_function(imp->addr, num_args_to_func, callstack, callstackcount);
		case IT_VARARGS1:
			return call_variadic_function_1arg (imp->addr, num_args_to_func, callstack, callstackcount);
		case IT_VARARGS2:
			return call_variadic_function_2args(imp->addr, num_args_to_func, callstack, callstackcount);
		case IT_VARARGS3:
			return call_variadic_function_3args(imp->addr, num_args_to_func, callstack, callstackcount);
		case IT_VARARGS4:
			return call_variadic_function_4args(imp->addr, num_args_to_func, callstack, callstackcount);
		case IT_VARARGS5:
			return call_variadic_function_5args(imp->addr, num_args_to_func, callstack, callstackcount);
		case IT_VARARGS6:
			return call_variadic_function_6args(imp->addr, num_args_to_func, callstack, callstackcount);
		case IT_VARARGS7:
			return call_variadic_function_7args(imp->addr, num_args_to_func, callstack, callstackcount);
		default:
			assert(0);
	}
}

#define MAX_FUNC_PARAMS 20
#define CHECK_STACK \
if ((inst->registers[SREG_SP] - ((long)&inst->stack[0])) >= CC_STACK_SIZE) { \
  cc_error("stack overflow"); \
  return -1; \
}

#define MAXNEST 50  // number of recursive function calls allowed
int cc_run_code(ccInstance * inst, long curpc) {
	#define regtype long
	#define REG(X) inst->registers[X]
	#define REGP(X) *((regtype *)(REG(X)))
	#define REGPI(X) *((int*)(REG(X)))
	//#define REGPI(X) REGP(X)
#if 1
#define POINTSTOSTACK ( \
                        (REG(SREG_MAR) <= REG(SREG_SP) + 21 * sizeof(long)) \
			&& (REG(SREG_MAR) >= REG(SREG_SP) - 21 * sizeof(long)) \
			)
#else
#define POINTSTOSTACK ( \
			( (unsigned long) REG(SREG_MAR) >= (unsigned long) callstack ) \
			&& ( (unsigned long) REG(SREG_MAR) <= (unsigned long) callstack + sizeof(callstack) ) \
			)
#endif
  inst->pc = curpc;
  inst->returnValue = -1;

  // 64 bit: For dealing with the stack
  int original_sp_diff = 0;
  int new_sp_diff = 0;
  int sp_index = 0;

  if ((curpc < 0) || (curpc >= inst->runningInst->codesize)) {
    cc_error("specified code offset is not valid");
    return -1;
  }

  // Needed to avoid unaligned variable access.
  long temp_variable = 0;

  long arg1 = 0, arg2 = 0;
  char *mptr;
  unsigned char tbyte;
  short tshort;
  
  long callstack[MAX_FUNC_PARAMS + 1];
  long thisbase[MAXNEST], funcstart[MAXNEST];
  int callstacksize = 0, aa, was_just_callas = -1;
  int curnest = 0;
  int loopIterations = 0;
  int num_args_to_func = -1;
  int next_call_needs_object = 0;
  int loopIterationCheckDisabled = 0;
  thisbase[0] = 0;
  funcstart[0] = inst->pc;
  current_instance = inst;
  float *freg1, *freg2;
  ccInstance *codeInst = inst->runningInst;
  unsigned long thisInstruction = 0, prevInstruction;
  int write_debug_dump = ccGetOption(SCOPT_DEBUGRUN);

  while (1) {
	  prevInstruction = thisInstruction;
    thisInstruction = codeInst->code[inst->pc] & INSTANCE_ID_REMOVEMASK;
    if (write_debug_dump)
      dump_instruction(&codeInst->code[inst->pc], inst->pc, inst->registers[SREG_SP]);

    // save the arguments for quick access
    if (inst->pc != (codeInst->codesize - 1)) {
      arg1 = codeInst->code[inst->pc + 1];
      freg1 = (float*)&inst->registers[arg1];
      if (inst->pc != (codeInst->codesize - 2)) {
        arg2 = codeInst->code[inst->pc + 2];
        freg2 = (float*)&inst->registers[arg2];
      }
    }

    switch (thisInstruction) {
      case SCMD_LINENUM:
        inst->line_number = arg1;
        currentline = arg1;
        if (new_line_hook)
          new_line_hook(inst, currentline);
        break;
      case SCMD_ADD:
#if defined(AGS_64BIT)
        // 64 bit: Keeping track of the stack variables
        if (arg1 == SREG_SP)
        {
          inst->stackSizes[inst->stackSizeIndex] = arg2;
          inst->stackSizeIndex++;
        }
#endif

        inst->registers[arg1] += arg2;
        CHECK_STACK 
        break;
      case SCMD_SUB:
#if defined(AGS_64BIT)
        // 64 bit: Rewrite the offset so that it doesn't point inside a variable on the stack.
        // AGS 2.x games also perform relative stack access by copying SREG_SP to SREG_MAR
        // and then subtracting from that.
        if ((arg1 == SREG_SP) || ((arg1 == SREG_MAR) && (inst->registers[SREG_MAR] == inst->registers[SREG_SP])))
        {
          int orig_sub = arg2;
          int new_sub = 0;
          int temp_index = inst->stackSizeIndex;
          while (orig_sub > 0)
          {
            if (temp_index < 1)
              cc_error("Subtracting from stack variable would underflow stack. Stack corrupted?");

            if (inst->stackSizes[temp_index - 1] == -1)
            {
              orig_sub -= 4;
              new_sub += sizeof(long);
            }
            else
            {
              orig_sub -= inst->stackSizes[temp_index - 1];
              new_sub += inst->stackSizes[temp_index - 1];
            }
            temp_index--;
          }
          if (arg1 == SREG_SP)
            inst->stackSizeIndex = temp_index;

          inst->registers[arg1] -= new_sub;
        }
        else
          inst->registers[arg1] -= arg2;
#else
          inst->registers[arg1] -= arg2;
#endif  
        break;
      case SCMD_REGTOREG:
        inst->registers[arg2] = inst->registers[arg1];
        break;
      case SCMD_WRITELIT:
        // poss something dodgy about this routine
        mptr = (char *)(inst->registers[SREG_MAR]);
	*((long*) mptr) = 0;
	assert(arg1 <= sizeof(long));
        memcpy(&mptr[0], &arg2, arg1);
        break;
      case SCMD_RET:
        if (loopIterationCheckDisabled > 0)
          loopIterationCheckDisabled--;

        REG(SREG_SP) -= sizeof(long);
#if defined(AGS_64BIT)
        inst->stackSizeIndex--;
#endif

        curnest--;
	inst->pc = REGP(SREG_SP);
        if (inst->pc == 0) {
          inst->returnValue = (int)inst->registers[SREG_AX];
          return 0;
        }
        current_instance = inst;
        POP_CALL_STACK(inst);
        continue;                 // continue so that the PC doesn't get overwritten
      case SCMD_LITTOREG:
        inst->registers[arg1] = arg2;
        break;
      case SCMD_MEMREAD:
#if 0 && defined(AGS_64BIT)
		if(prevInstruction != SCMD_LOADSPOFFS || !POINTSTOSTACK || inst->stackSizes[inst->stackSizeIndex - 1] != -1
			|| (unsigned long) REGP(SREG_MAR) < 0xFFFFffff
		) {
		//if(!POINTSTOSTACK || inst->stackSizes[inst->stackSizeIndex - 1] != -1) {
#endif
			REG(arg1) = 0; // bogus zero mem in case only a half word gets written...
			REG(arg1) = REGPI(SREG_MAR);
#if 0 && defined(AGS_64BIT)
		} else {
		      REG(arg1) = REGP(SREG_MAR);
		}
#endif
	      // if next instruction = meminit.ptr we must read sizeof long
        break;
      case SCMD_MEMWRITE:
		//if(!POINTSTOSTACK) 
			REGPI(SREG_MAR) = REG(arg1);
		//else 
		//	REGP(SREG_MAR) = REG(arg1);
        break;
      case SCMD_LOADSPOFFS:
#if defined(AGS_64BIT)
        // 64 bit: Rewrite offset so that it doesn't point inside a variable
        original_sp_diff = arg1;
        new_sp_diff = 0;
        sp_index = inst->stackSizeIndex - 1;

        while (original_sp_diff > 0)
        {
          if (inst->stackSizes[sp_index] == -1)
          {
           	original_sp_diff -= 4;
            new_sp_diff += sizeof(long);//inst->stackSizes[sp_index];
            sp_index--;
          }
          else
          {
            original_sp_diff -= inst->stackSizes[sp_index];
            new_sp_diff += inst->stackSizes[sp_index];
            sp_index--;
          }
        }

        if (sp_index < -1)
          cc_error("Stack offset cannot be rewritten. Stack corrupted?");

        inst->registers[SREG_MAR] = inst->registers[SREG_SP] - new_sp_diff;
#else
        REG(SREG_MAR) = REG(SREG_SP) -arg1;
#endif
        break;

      // 64 bit: Force 32 bit math

      case SCMD_MULREG:
	      REG(arg1) = REG(arg1) * REG(arg2);
        break;
      case SCMD_DIVREG:
        if (inst->registers[arg2] == 0) {
          cc_error("!Integer divide by zero");
          return -1;
        }
        REG(arg1) = REG(arg1) / REG(arg2);
        break;
      case SCMD_ADDREG:
	      REG(arg1) = REG(arg1) + REG(arg2);
        break;
      case SCMD_SUBREG:
	      REG(arg1) = REG(arg1) - REG(arg2);
        break;
      case SCMD_BITAND:
	      REG(arg1) = REG(arg1) & REG(arg2);
        break;
      case SCMD_BITOR:
	      REG(arg1) = REG(arg1) | REG(arg2);
        break;
      case SCMD_ISEQUAL:
	      REG(arg1) = REG(arg1) == REG(arg2);
        break;
      case SCMD_NOTEQUAL:
	      REG(arg1) = REG(arg1) != REG(arg2);
        break;
      case SCMD_GREATER:
	      REG(arg1) = REG(arg1) > REG(arg2);
        break;
      case SCMD_LESSTHAN:
	      REG(arg1) = REG(arg1) < REG(arg2);
        break;
      case SCMD_GTE:
	      REG(arg1) = REG(arg1) >= REG(arg2);
        break;
      case SCMD_LTE:
	      REG(arg1) = REG(arg1) <= REG(arg2);
        break;
      case SCMD_AND:
	      REG(arg1) = REG(arg1) && REG(arg2);
        break;
      case SCMD_OR:
	      REG(arg1) = REG(arg1) || REG(arg2);
        break;
      case SCMD_XORREG:
	      REG(arg1) = REG(arg1) ^ REG(arg2);
        break;
      case SCMD_MODREG:
        if (inst->registers[arg2] == 0) {
          cc_error("!Integer divide by zero");
          return -1;
        }
        REG(arg1) = REG(arg1) % REG(arg2);
        break;
      case SCMD_NOTREG:
	      REG(arg1) = !REG(arg1);
        break;
      case SCMD_CALL:
        // Call another function within same script, just save PC
        // and continue from there
        if (curnest >= MAXNEST - 1) {
          cc_error("!call stack overflow, recursive call problem?");
          return -1;
        }

        PUSH_CALL_STACK(inst);

        temp_variable = inst->pc + sccmdargs[thisInstruction] + 1;
	REGP(SREG_SP) = temp_variable;
	REG(SREG_SP) += sizeof(regtype);

#if defined(AGS_64BIT)
        inst->stackSizes[inst->stackSizeIndex] = -1;
        inst->stackSizeIndex++;
#endif

        if (thisbase[curnest] == 0)
          inst->pc = inst->registers[arg1];
        else {
          inst->pc = funcstart[curnest];
          inst->pc += (inst->registers[arg1] - thisbase[curnest]);
        }

        if (next_call_needs_object)  // is this right?
          next_call_needs_object = 0;

        if (loopIterationCheckDisabled)
          loopIterationCheckDisabled++;

        curnest++;
        thisbase[curnest] = 0;
        funcstart[curnest] = inst->pc;
        CHECK_STACK
        continue;
      case SCMD_MEMREADB:
        tbyte = *((unsigned char *)inst->registers[SREG_MAR]);
        inst->registers[arg1] = tbyte;
        break;
      case SCMD_MEMREADW:
        tshort = *((short *)inst->registers[SREG_MAR]);
        inst->registers[arg1] = tshort;
        break;
      case SCMD_MEMWRITEB:
	      *((unsigned char*) inst->registers[SREG_MAR]) = inst->registers[arg1] & 0xFF;
        break;
      case SCMD_MEMWRITEW:
	      *((short*) inst->registers[SREG_MAR]) = inst->registers[arg1] & 0xFFFF;
        break;
      case SCMD_JZ:
        if (inst->registers[SREG_AX] == 0)
          inst->pc += arg1;
        break;
      case SCMD_JNZ:
        if (inst->registers[SREG_AX] != 0)
          inst->pc += arg1;
        break;
      case SCMD_PUSHREG:
#if defined(AGS_64BIT)
        // 64 bit: Registers are pushed as 8 byte values. Their size is set to "-1" so that
        // they can be identified later.
        inst->stackSizes[inst->stackSizeIndex] = -1;
        inst->stackSizeIndex++;
#endif

        REGP(SREG_SP) = REG(arg1);
	REG(SREG_SP) += sizeof(regtype);
        CHECK_STACK
        break;
      case SCMD_POPREG:
#if defined(AGS_64BIT)
        // 64 bit: Registers are pushed as 8 byte values
        if (inst->stackSizes[inst->stackSizeIndex - 1] != -1)
          cc_error("Trying to pop value that was not pushed. Stack corrupted?");

        inst->stackSizeIndex--;
#endif
	// FIXME imo the the sp should be decreased after the pop
        REG(SREG_SP) -= sizeof(regtype);
	REG(arg1) = REGP(SREG_SP);
        break;
      case SCMD_JMP:
        inst->pc += arg1;
        if ((arg1 < 0) && (maxWhileLoops > 0) && (loopIterationCheckDisabled == 0)) {
          // Make sure it's not stuck in a While loop
          loopIterations ++;
          if (inst->flags & INSTF_RUNNING) {
            loopIterations = 0;
            inst->flags &= ~INSTF_RUNNING;
          }
          else if (loopIterations > maxWhileLoops) {
            cc_error("!Script appears to be hung (a while loop ran %d times). The problem may be in a calling function; check the call stack.", loopIterations);
            return -1;
          }
        }
        break;
      case SCMD_MUL:
        inst->registers[arg1] *= arg2;
        break;
      case SCMD_CHECKBOUNDS:
        if ((inst->registers[arg1] < 0) ||
            (inst->registers[arg1] >= arg2)) {
          cc_error("!Array index out of bounds (index: %d, bounds: 0..%d)", inst->registers[arg1], arg2 - 1);
          return -1;
        }
        break;
      case SCMD_DYNAMICBOUNDS:
        {
		// XXXXXXXXX very bogus
		// FIXME cross- check with dynamicarray thingie
        long upperBoundInBytes = *((int *)(inst->registers[SREG_MAR] - 4));
        if ((inst->registers[arg1] < 0) ||
            (inst->registers[arg1] >= upperBoundInBytes)) {
          long upperBound = *((int *)(inst->registers[SREG_MAR] - 8)) & (~ARRAY_MANAGED_TYPE_FLAG);
          int elementSize = (upperBoundInBytes / upperBound);
          cc_error("!Array index out of bounds (index: %d, bounds: 0..%d)", inst->registers[arg1] / elementSize, upperBound - 1);
          return -1;
        }
        break;
        }

      // 64 bit: Handles are always 32 bit values. They are not C pointer.

      case SCMD_MEMREADPTR: {
        ccError = 0;

        long handle = REGPI(SREG_MAR);
	REG(arg1) = (long)ccGetObjectAddressFromHandle(handle);

        // if error occurred, cc_error will have been set
        if (ccError)
          return -1;
        break;
      }
      case SCMD_MEMWRITEPTR: {

        long handle = REGPI(SREG_MAR);

        long newHandle = ccGetObjectHandleFromAddress((char*)REG(arg1));
        if (newHandle == -1)
          return -1;

        if (handle != newHandle) {
          ccReleaseObjectReference(handle);
          ccAddObjectReference(newHandle);
	  REGPI(SREG_MAR) = newHandle;
        }
        break;
      }
      case SCMD_MEMINITPTR: { 
        // like memwriteptr, but doesn't attempt to free the old one

        long handle = REGPI(SREG_MAR);

        long newHandle = ccGetObjectHandleFromAddress((char*)REG(arg1));
        if (newHandle == -1)
          return -1;

        ccAddObjectReference(newHandle);
	REGPI(SREG_MAR) = newHandle;
        break;
      }
      case SCMD_MEMZEROPTR: {
        long handle = REGPI(SREG_MAR);
        ccReleaseObjectReference(handle);
	REGPI(SREG_MAR) = 0;
        break;
      }
      case SCMD_MEMZEROPTRND: {
        long handle = REGPI(SREG_MAR);

        // don't do the Dispose check for the object being returned -- this is
        // for returning a String (or other pointer) from a custom function.
        // Note: we might be freeing a dynamic array which contains the DisableDispose
        // object, that will be handled inside the recursive call to SubRef.
        pool.disableDisposeForObject = (const char*)REG(SREG_AX);
        ccReleaseObjectReference(handle);
        pool.disableDisposeForObject = NULL;
	REGPI(SREG_MAR) = 0;
        break;
      }
      case SCMD_CHECKNULL:
        if (inst->registers[SREG_MAR] == 0) {
          cc_error("!Null pointer referenced");
          return -1;
        }
        break;
      case SCMD_CHECKNULLREG:
        if (inst->registers[arg1] == 0) {
          cc_error("!Null string referenced");
          return -1;
        }
        break;
      case SCMD_NUMFUNCARGS:
        num_args_to_func = arg1;
        break;
      case SCMD_CALLAS:{
        PUSH_CALL_STACK(inst);

        // Call to a function in another script
        int startArg = 0;
        // If there are nested CALLAS calls, the stack might
        // contain 2 calls worth of parameters, so only
        // push args for this call
        if (num_args_to_func >= 0)
          startArg = callstacksize - num_args_to_func;

        for (aa = startArg; aa < callstacksize; aa++) {
          // 64 bit: Arguments are pushed as 64 bit values
          REGP(SREG_SP) = callstack[aa];
	  REG(SREG_SP) += sizeof(regtype);

#if defined(AGS_64BIT)
          inst->stackSizes[inst->stackSizeIndex] = -1;//sizeof(long);
          inst->stackSizeIndex++;
#endif
        }

        // 0, so that the cc_run_code returns
        REGP(SREG_SP) = 0;

        long oldstack = REG(SREG_SP);
        REG(SREG_SP) += sizeof(regtype);
        CHECK_STACK

#if defined(AGS_64BIT)
        inst->stackSizes[inst->stackSizeIndex] = -1;//sizeof(long);
        inst->stackSizeIndex++;
#endif

        int oldpc = inst->pc;
        ccInstance *wasRunning = inst->runningInst;

        // extract the instance ID
        unsigned long instId = (codeInst->code[inst->pc] >> INSTANCE_ID_SHIFT) & INSTANCE_ID_MASK;
        // determine the offset into the code of the instance we want
        inst->runningInst = loadedInstances[instId];
        unsigned long callAddr = inst->registers[arg1] - (unsigned long)(&inst->runningInst->code[0]);
        if (callAddr % 4 != 0) {
          cc_error("call address not aligned");
          return -1;
        }
        callAddr /= sizeof(long);

        if (cc_run_code(inst, callAddr))
          return -1;

        inst->runningInst = wasRunning;

        if (inst->flags & INSTF_ABORTED)
          return 0;

        if (oldstack != inst->registers[SREG_SP]) {
          cc_error("stack corrupt after function call");
          return -1;
        }

        if (next_call_needs_object)
          next_call_needs_object = 0;
  
        inst->pc = oldpc;
        was_just_callas = callstacksize;
        num_args_to_func = -1;
        POP_CALL_STACK(inst);
        break;
      }
      case SCMD_CALLEXT: {
        int call_uses_object = 0;
        // Call to a real 'C' code function
	//char* funcname = ccGetNameForFunc(inst->registers[arg1]);
	struct ImportInfo * imp = (struct ImportInfo *) inst->registers[arg1];
        was_just_callas = -1;
        if (num_args_to_func < 0)
          num_args_to_func = callstacksize;

        if (next_call_needs_object) {
          // member function call
          // use the callstack +1 size allocation to squeeze
          // the object address on as the last parameter
          call_uses_object = 1;
          next_call_needs_object = 0;
          callstack[callstacksize] = inst->registers[SREG_OP];
#ifdef DEBUG_FUNCTION_CALLS
	  dprintf(2, "calling obj func: %s with %d args\n", funcname, num_args_to_func+1);
#endif
	  inst->registers[SREG_AX] = do_function_call(imp, num_args_to_func + 1, callstack, callstacksize - num_args_to_func);
        } else if (num_args_to_func == 0) {
#ifdef DEBUG_FUNCTION_CALLS
		dprintf(2, "calling no-arg func: %s\n", funcname);
#endif
		//long (*realfunc) (void);
		//realfunc = (long (*)(void))inst->registers[arg1];
		inst->registers[SREG_AX] = do_function_call(imp, 0, 0, 0); // realfunc();
        } else {
#ifdef DEBUG_FUNCTION_CALLS
		dprintf(2, "calling regular func: %s with %d args\n", funcname, num_args_to_func);
#endif
		inst->registers[SREG_AX] = do_function_call(imp, num_args_to_func, callstack, callstacksize - num_args_to_func);
	}

        if (ccError)
          return -1;

        if (call_uses_object) {
          // Pop OP?
        }

        current_instance = inst;
        num_args_to_func = -1;
        break;
      }
      case SCMD_PUSHREAL:
//        printf("pushing arg%d as %ld\n",callstacksize,inst->registers[arg1]);
        if (callstacksize >= MAX_FUNC_PARAMS) {
          cc_error("Call stack overflow");
          return -1;
        }
        callstack[callstacksize] = inst->registers[arg1];
        callstacksize++;
        break;
      case SCMD_SUBREALSTACK:
        if (was_just_callas >= 0) {
          inst->registers[SREG_SP] -= arg1 * sizeof(long);
#if defined(AGS_64BIT)
          inst->stackSizeIndex -= arg1;
#endif
          was_just_callas = -1;
        }
        callstacksize -= arg1;
        break;
      case SCMD_CALLOBJ:
        // set the OP register
        if (inst->registers[arg1] == 0) {
          cc_error("!Null pointer referenced");
          return -1;
        }
        inst->registers[SREG_OP] = inst->registers[arg1];
        next_call_needs_object = 1;
        break;
      case SCMD_SHIFTLEFT:
	      REG(arg1) = REG(arg1) << (REG(arg2) & 31);
        break;
      case SCMD_SHIFTRIGHT:
	      REG(arg1) = REG(arg1) >> (REG(arg2) & 31);
        break;
      case SCMD_THISBASE:
        thisbase[curnest] = arg1;
        break;
      case SCMD_NEWARRAY:
        {
		// FIXME entire dyn array stuff is bogus
        long arg3 = codeInst->code[inst->pc + 3];
        long numElements = inst->registers[arg1];
        if ((numElements < 1) || (numElements > 1000000))
        {
          cc_error("invalid size for dynamic array; requested: %d, range: 1..1000000", numElements);
          return -1;
        }
        inst->registers[arg1] = (long)ccGetObjectAddressFromHandle(globalDynamicArray.Create(numElements, arg2, (arg3 == 1)));
        break;
        }
      case SCMD_FADD:
        *freg1 += arg2;
        break;
      case SCMD_FSUB:
        *freg1 -= arg2;
        break;
      case SCMD_FMULREG:
        freg1[0] *= freg2[0];
        break;
      case SCMD_FDIVREG:
        if (freg2[0] == 0.0) {
          cc_error("!Floating point divide by zero");
          return -1;
        } 
        freg1[0] /= freg2[0];
        break;
      case SCMD_FADDREG:
        freg1[0] += freg2[0];
        break;
      case SCMD_FSUBREG:
        freg1[0] -= freg2[0];
        break;
      case SCMD_FGREATER:
        freg1[0] = (freg1[0] > freg2[0]);
        break;
      case SCMD_FLESSTHAN:
        freg1[0] = (freg1[0] < freg2[0]);
        break;
      case SCMD_FGTE:
        freg1[0] = (freg1[0] >= freg2[0]);
        break;
      case SCMD_FLTE:
        freg1[0] = (freg1[0] <= freg2[0]);
        break;
      case SCMD_ZEROMEMORY:
        mptr = (char *)(inst->registers[SREG_MAR]);
        if (inst->registers[SREG_MAR] == inst->registers[SREG_SP]) {
          // creating a local variable -- check the stack to ensure no mem overrun
          int currentStackSize = inst->registers[SREG_SP] - ((long)&inst->stack[0]);
          if (currentStackSize + arg1 >= CC_STACK_SIZE) {
            cc_error("stack overflow, attempted grow to %d bytes", currentStackSize + arg1);
            return -1;
          }
        }
        memset(mptr, 0, arg1);
        break;
      case SCMD_CREATESTRING:
        if (stringClassImpl == NULL) {
          cc_error("No string class implementation set, but opcode was used");
          return -1;
        }
        inst->registers[arg1] = (long)stringClassImpl->CreateString((const char *)(inst->registers[arg1]));
        break;
      case SCMD_STRINGSEQUAL:
        if ((inst->registers[arg1] == 0) || (inst->registers[arg2] == 0)) {
          cc_error("!Null pointer referenced");
          return -1;
        }
        if (strcmp((const char*)inst->registers[arg1], (const char*)inst->registers[arg2]) == 0)
          inst->registers[arg1] = 1;
        else
          inst->registers[arg1] = 0;
        break;
      case SCMD_STRINGSNOTEQ:
        if ((inst->registers[arg1] == 0) || (inst->registers[arg2] == 0)) {
          cc_error("!Null pointer referenced");
          return -1;
        }
        if (strcmp((const char*)inst->registers[arg1], (const char*)inst->registers[arg2]) != 0)
          inst->registers[arg1] = 1;
        else
          inst->registers[arg1] = 0;
        break;
      case SCMD_LOOPCHECKOFF:
        if (loopIterationCheckDisabled == 0)
          loopIterationCheckDisabled++;
        break;
      default:
        cc_error("invalid instruction %d found in code stream", thisInstruction);
        return -1;
    }

    if (inst->flags & INSTF_ABORTED)
      return 0;

    inst->pc += sccmdargs[thisInstruction] + 1;
  }
}

int ccCallInstance(ccInstance * inst, char *funcname, long numargs, ...)
{
  ccError = 0;
  currentline = 0;

  if ((numargs >= 20) || (numargs < 0)) {
    cc_error("too many arguments to function");
    return -3;
  }

  if (inst->pc != 0) {
    cc_error("instance already being executed");
    return -4;
  }

  long startat = -1;
  int k;
  char mangledName[200];
  sprintf(mangledName, "%s$", funcname);

  for (k = 0; k < inst->instanceof->numexports; k++) {
    char *thisExportName = inst->instanceof->exports[k];
    int match = 0;

    // check for a mangled name match
    if (strncmp(thisExportName, mangledName, strlen(mangledName)) == 0) {
      // found, compare the number of parameters
      char *numParams = thisExportName + strlen(mangledName);
      if (atoi(numParams) != numargs) {
        cc_error("wrong number of parameters to exported function '%s' (expected %d, supplied %d)", funcname, atoi(numParams), numargs);
        return -1;
      }
      match = 1;
    }
    // check for an exact match (if the script was compiled with
    // an older version)
    if ((match == 1) || (strcmp(thisExportName, funcname) == 0)) {
      long etype = (inst->instanceof->export_addr[k] >> 24L) & 0x000ff;
      if (etype != EXPORT_FUNCTION) {
        cc_error("symbol is not a function");
        return -1;
      }
      startat = (inst->instanceof->export_addr[k] & 0x00ffffff);
      break;
    }
  }

  if (startat < 0) {
    cc_error("function '%s' not found", funcname);
    return -2;
  }

  long tempstack[20];
  int tssize = 1;
  tempstack[0] = 0;             // return address on stack
  if (numargs > 0) {
    va_list ap;
    va_start(ap, numargs);
    while (tssize <= numargs) {
      tempstack[tssize] = va_arg(ap, long);
      tssize++;
    }
    va_end(ap);
  }
  numargs++;                    // account for return address
  inst->flags &= ~INSTF_ABORTED;

  // object pointer needs to start zeroed
  inst->registers[SREG_OP] = 0;

  ccInstance* currentInstanceWas = current_instance;
  long stoffs = 0;
  for (tssize = numargs - 1; tssize >= 0; tssize--) {
    memcpy(&inst->stack[stoffs], &tempstack[tssize], sizeof(long));
    stoffs += sizeof(long);
  }
  inst->registers[SREG_SP] = (long)(&inst->stack[0]);
  inst->registers[SREG_SP] += (numargs * sizeof(long));
  inst->runningInst = inst;

#if defined(AGS_64BIT)
  // 64 bit: Initialize array for stack variable sizes with the argument values
  inst->stackSizeIndex = 0;
  int i;
  for (i = 0; i < numargs; i++)
  {
    inst->stackSizes[inst->stackSizeIndex] = -1;
    inst->stackSizeIndex++;
  }
#endif

  int reterr = cc_run_code(inst, startat);
  inst->registers[SREG_SP] -= (numargs - 1) * sizeof(long);
  inst->pc = 0;
  current_instance = currentInstanceWas;

  // NOTE that if proper multithreading is added this will need
  // to be reconsidered, since the GC could be run in the middle 
  // of a RET from a function or something where there is an 
  // object with ref count 0 that is in use
  pool.RunGarbageCollectionIfAppropriate();

  if (new_line_hook)
    new_line_hook(NULL, 0);

  if (reterr)
    return -6;

  if (inst->flags & INSTF_ABORTED) {
    inst->flags &= ~INSTF_ABORTED;

    if (inst->flags & INSTF_FREE)
      ccFreeInstance(inst);
    return 100;
  }

  if (inst->registers[SREG_SP] != (long)&inst->stack[0]) {
    cc_error("stack pointer was not zero at completion of script");
    return -5;
  }
  return ccError;
}

void ccAbortInstance(ccInstance * cinst)
{
  if ((cinst != NULL) && (cinst->pc != 0))
    cinst->flags |= INSTF_ABORTED;
}

void ccAbortAndDestroyInstance(ccInstance * inst)
{
  if (inst != NULL) {
    ccAbortInstance(inst);
    inst->flags |= INSTF_FREE;
  }
}

void freadstring(char **strptr, FILE * iii)
{
  static char ibuffer[300];
  int idxx = 0;

  while ((ibuffer[idxx] = fgetc(iii)) != 0)
    idxx++;

  if (ibuffer[0] == 0) {
    strptr[0] = NULL;
    return;
  }

  strptr[0] = (char *)malloc(strlen(ibuffer) + 1);
  strcpy(strptr[0], ibuffer);
}

// 64 bit: This is supposed to read a 32 bit value
int fget_long(FILE * iii)
{
  int tmpp;
  fread(&tmpp, 4, 1, iii);
  return tmpp;
}

ccScript *fread_script(FILE * ooo)
{
  ccScript *scri = (ccScript *) malloc(sizeof(ccScript));
  scri->instances = 0;
  int n;
  char gotsig[5];
  currentline = -1;
  // MACPORT FIX: swap 'size' and 'nmemb'
  fread(gotsig, 1, 4, ooo);
  gotsig[4] = 0;

  int fileVer = fget_long(ooo);

  if ((strcmp(gotsig, scfilesig) != 0) || (fileVer > SCOM_VERSION)) {
    cc_error("file was not written by fwrite_script or seek position is incorrect");
    free(scri);
    return NULL;
  }

  scri->globaldatasize = fget_long(ooo);
  scri->codesize = fget_long(ooo);
  scri->stringssize = fget_long(ooo);

  if (scri->globaldatasize > 0) {
    scri->globaldata = (char *)malloc(scri->globaldatasize);
    // MACPORT FIX: swap
    fread(scri->globaldata, sizeof(char), scri->globaldatasize, ooo);
  }
  else
    scri->globaldata = NULL;

  if (scri->codesize > 0) {
    scri->code = (long *)malloc(scri->codesize * sizeof(long));
    // MACPORT FIX: swap

    // 64 bit: Read code into 8 byte array, necessary for being able to perform
    // relocations on the references.
    int i;
    for (i = 0; i < scri->codesize; i++)
      scri->code[i] = fget_long(ooo);
  }
  else
    scri->code = NULL;

  if (scri->stringssize > 0) {
    scri->strings = (char *)malloc(scri->stringssize);
    // MACPORT FIX: swap
    fread(scri->strings, sizeof(char), scri->stringssize, ooo);
  } 
  else
    scri->strings = NULL;

  scri->numfixups = fget_long(ooo);
  if (scri->numfixups > 0) {
    scri->fixuptypes = (char *)malloc(scri->numfixups);
    scri->fixups = (long *)malloc(scri->numfixups * sizeof(long));
    // MACPORT FIX: swap 'size' and 'nmemb'
    fread(scri->fixuptypes, sizeof(char), scri->numfixups, ooo);

    // 64 bit: Read fixups into 8 byte array too
    int i;
    for (i = 0; i < scri->numfixups; i++)
      scri->fixups[i] = fget_long(ooo);
  }
  else {
    scri->fixups = NULL;
    scri->fixuptypes = NULL;
  }

  scri->numimports = fget_long(ooo);

  scri->imports = (char**)malloc(sizeof(char*) * scri->numimports);
  for (n = 0; n < scri->numimports; n++)
    freadstring(&scri->imports[n], ooo);

  scri->numexports = fget_long(ooo);
  scri->exports = (char**)malloc(sizeof(char*) * scri->numexports);
  scri->export_addr = (long*)malloc(sizeof(long) * scri->numexports);
  for (n = 0; n < scri->numexports; n++) {
    freadstring(&scri->exports[n], ooo);
    scri->export_addr[n] = fget_long(ooo);
  }

  if (fileVer >= 83) {
    // read in the Sections
    scri->numSections = fget_long(ooo);
    scri->sectionNames = (char**)malloc(scri->numSections * sizeof(char*));
    scri->sectionOffsets = (long*)malloc(scri->numSections * sizeof(long));
    for (n = 0; n < scri->numSections; n++) {
      freadstring(&scri->sectionNames[n], ooo);
      scri->sectionOffsets[n] = fget_long(ooo);
    }
  }
  else
  {
    scri->numSections = 0;
    scri->sectionNames = NULL;
    scri->sectionOffsets = NULL;
  }

  if (fget_long(ooo) != ENDFILESIG) {
    cc_error("internal error rebuilding script");
    free(scri);
    return NULL;
  }
  return scri;
}
