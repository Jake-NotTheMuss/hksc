/*
** $Id: lua.h $
** Lua - An Extensible Extension Language
** Lua.org, PUC-Rio, Brazil (http://www.lua.org)
** See Copyright Notice at the end of this file
*/


#ifndef lua_h
#define lua_h

#include <stdarg.h>
#include <stddef.h>


#include "hkscluaconf.h"


#define LUA_VERSION	"Lua 5.1"
#define LUA_RELEASE "Lua 5.1.4"
#define LUA_VERSION_NUM	501
#define LUA_COPYRIGHT "Copyright (C) 1994-2008 Lua.org, PUC-Rio"
#define LUA_AUTHORS   "R. Ierusalimschy, L. H. de Figueiredo & W. Celes"

#define HKSC_VERSION "0.0.0"

/* mark for precompiled code (`<esc>Lua') */
#define	LUA_SIGNATURE	"\033Lua"

/* option for multiple returns in `lua_pcall' and `lua_call' */
#define LUA_MULTRET	(-1)


/* thread status; 0 is OK */
#define LUA_YIELD	1
#define LUA_ERRRUN	2
#define LUA_ERRSYNTAX	3
#define LUA_ERRMEM	4
#define LUA_ERRERR	5


typedef struct hksc_State hksc_State;

typedef int (*lua_CFunction) (hksc_State *H);


#ifdef HKSC_MULTIPLAT
/* putany targets that Havok Script is used on */
#define HKSC_TARGET_PLAT_DEFAULT 0 /* the host platform */
#define HKSC_TARGET_PLAT_WII 1
#define HKSC_TARGET_PLAT_CAFE 2
#define HKSC_TARGET_PLAT_NX 3
#define HKSC_TARGET_PLAT_PS3 4
#define HKSC_TARGET_PLAT_PSV 5
#define HKSC_TARGET_PLAT_ORBIS 6
#define HKSC_TARGET_PLAT_XENON 7
#define HKSC_TARGET_PLAT_DURANGO 8
#define HKSC_TARGET_PLAT_WINDOWS 9
#define HKSC_TARGET_PLAT_GNU 10
#define HKSC_TARGET_PLAT_DARWIN 11

/* the last of the embedded devices */
#define HKSC_TARGET_PLAT_LAST_EMBEDDED HKSC_TARGET_PLAT_DURANGO

#define HKSC_TARGET_WS_DEFAULT 0  /* host word-size */
#define HKSC_TARGET_WS_16 1
#define HKSC_TARGET_WS_32 2
#define HKSC_TARGET_WS_64 3
#define HKSC_TARGET_WS_COUNT 4
#endif /* HKSC_MULTIPLAT */

/* user-defined callbacks for beginning/end of cycles */
typedef void (*hksc_CycleCallback)(hksc_State *H, const char *name);


/*
** functions that read/write blocks when loading/dumping Lua chunks
*/
typedef const char * (*lua_Reader) (hksc_State *H, void *ud, size_t *sz);

typedef int (*lua_Writer) (hksc_State *H, const void* p, size_t sz, void* ud);


/*
** prototype for memory-allocation functions
*/
typedef void * (*lua_Alloc) (void *ud, void *ptr, size_t osize, size_t nsize);


/*
** basic types
*/
#define LUA_TANY    (-2)
#define LUA_TNONE		(-1)

#define LUA_TNIL		0
#define LUA_TBOOLEAN		1
#define LUA_TLIGHTUSERDATA	2
#define LUA_TNUMBER		3
#define LUA_TSTRING		4
#define LUA_TTABLE		5
#define LUA_TFUNCTION		6
#define LUA_TUSERDATA		7
#define LUA_TTHREAD		8
#define LUA_TIFUNCTION  9
#define LUA_TCFUNCTION  10
#define LUA_TUI64       11
#define LUA_TSTRUCT     12

#define LUA_NUM_TYPE_OBJECTS 14


/*
** int literal options
*/
#define INT_LITERALS_NONE   0 /* disable all literals */
#define INT_LITERALS_LUD    1 /* enable LUD literal constants */
#define INT_LITERALS_32BIT  INT_LITERALS_LUD
#define INT_LITERALS_UI64   2 /* enable UI64 literal constants */
#define INT_LITERALS_64BIT  INT_LITERALS_UI64
#define INT_LITERALS_ALL    3 /* enable all literals */


/*
** bytecode sharing modes
*/
#define HKSC_SHARING_MODE_OFF  0
#define HKSC_SHARING_MODE_ON   1
#define HKSC_SHARING_MODE_SECURE 2


/*
** bytecode sharing formats
*/
#define HKSC_BYTECODE_DEFAULT  0
#define HKSC_BYTECODE_INPLACE  1
#define HKSC_BYTECODE_REFERENCED 2

/*
** bytecode endianness
*/
#define HKSC_DEFAULT_ENDIAN   0
#define HKSC_BIG_ENDIAN       1
#define HKSC_LITTLE_ENDIAN    2

/*
** hksc modes
*/
#define HKSC_MODE_DEFAULT   0 /* infer from content of first file */
#define HKSC_MODE_SOURCE   1 /* compiling source */
#define HKSC_MODE_BINARY 2 /* loading bytecode */


/*
** bytecode stripping levels
*/
#define BYTECODE_STRIPPING_NONE 0
#define BYTECODE_STRIPPING_PROFILING 1
#define BYTECODE_STRIPPING_ALL 2
#ifdef LUA_CODT6 /* Cod extensions */
#define BYTECODE_STRIPPING_DEBUG_ONLY 3
#define BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION 4
#endif /* LUA_CODT6 */
 
/*
** Lua compiler settings (to be specified by the host program)
*/
typedef struct {
#ifdef LUA_CODT6
  int hash_step;
#endif
  /* general settings */
  int ignore_debug; /* do not try to load/dump debug info */
  /* compiler-specific settings */
  int emit_struct; /* whether `hstructure' and `hmake' should be emitted */
  int enable_int_literals; /* int literal setting */
#if HKSC_GETGLOBAL_MEMOIZATION
  int emit_memo;
  int skip_memo;
#endif /* HKSC_GETGLOBAL_MEMOIZATION */
  int strip; /* bytecode stripping level */
  /*const char **strip_names;*/
  /*lua_LineMap debug_map;*/
#ifdef HKSC_DECOMPILER /* decompiler-specific settings */
  int match_line_info; /* emit statements according to the line mapping */
#endif /* HKSC_DECOMPILER */
} hksc_CompilerSettings;


/*
** Lua state settings (to be specified by the host program)
*/
typedef struct {
  lua_Alloc frealloc;  /* function to reallocate memory */
  void *ud;         /* auxiliary data to `frealloc' */
  lua_CFunction panic;  /* to be called in unprotected errors */
  int mode;  /* what mode to run in (compiling/decompiling) */
  int bytecode_endianness;
#ifdef HKSC_MULTIPLAT
  int target_plat;
  int target_ws;
#endif /* HKSC_MULTIPLAT */
  hksc_CompilerSettings compilersettings;
} hksc_StateSettings;


/*
** generic extra include file
*/
#if defined(LUA_USER_H)
#include LUA_USER_H
#endif


/* type of numbers in Lua */
typedef LUA_NUMBER lua_Number;


/* type for integer functions */
typedef LUA_INTEGER lua_Integer;


/*
** state manipulation
*/
LUA_API hksc_State *(lua_newstate) (hksc_StateSettings *settings);
LUA_API void       (lua_close) (hksc_State *H);

LUA_API lua_CFunction (lua_atpanic) (hksc_State *H, lua_CFunction panicf);
LUA_API hksc_CycleCallback (lua_onstartcycle) (hksc_State *H,
                                               hksc_CycleCallback f);
LUA_API hksc_CycleCallback (lua_onendcycle) (hksc_State *H,
                                             hksc_CycleCallback f);


/*
** miscellaneous functions
*/
LUA_API const char *(lua_newfixedstring) (hksc_State *H, const char *str);
LUA_API const char *(lua_newfixedlstring) (hksc_State *H, const char *str,
                                           size_t l);
LUA_API const char *(lua_newstring) (hksc_State *H, const char *str);
LUA_API const char *(lua_newlstring) (hksc_State *H, const char *str, size_t l);
LUA_API const char *(lua_newfstring) (hksc_State *H, const char *fmt, ...);
LUA_API const char *(lua_newvfstring) (hksc_State *H, const char *fmt,
                                       va_list argp);
LUA_API const char *(lua_geterror) (hksc_State *H);
LUA_API void (lua_seterror) (hksc_State *H, const char *s);
LUA_API void (lua_setferror) (hksc_State *H, const char *fmt, ...);
LUA_API void (lua_setvferror) (hksc_State *H, const char *fmt, va_list argp);
LUA_API void (lua_clearerror) (hksc_State *H);
LUA_API int (lua_getmode) (hksc_State *H);
LUA_API void (lua_setmode) (hksc_State *H, int mode);
LUA_API lua_Alloc (lua_getallocf) (hksc_State *H, void **ud);
LUA_API void (lua_setallocf) (hksc_State *H, lua_Alloc f, void *ud);

LUA_API void (lua_setprefixmap) (hksc_State *H, const char *from,
                                 const char *to);

#if defined(LUA_CODT6)
LUA_API const char *(lua_getdebugfile) (hksc_State *H);
LUA_API void (lua_setdebugfile) (hksc_State *H, const char *name);
#endif /* LUA_CODT6 */

/*
** Lua compiler/decompiler settings
*/
LUA_API int (lua_getemitstruct) (hksc_State *H);
LUA_API void (lua_setemitstruct) (hksc_State *H, int emit_struct);
LUA_API int (lua_getintliteralsenabled) (hksc_State *H);
LUA_API void (lua_setintliteralsenabled) (hksc_State *H,
                                          int enable_int_literals);
LUA_API int (lua_getbytecodestrippinglevel) (hksc_State *H);
LUA_API void (lua_setbytecodestrippinglevel) (hksc_State *H, int strip);
LUA_API int (lua_getignoredebug) (hksc_State *H);
LUA_API void (lua_setignoredebug) (hksc_State *H, int ignore_debug);
#ifdef HKSC_DECOMPILER
LUA_API int (lua_getmatchlineinfo) (hksc_State *H);
LUA_API void (lua_setmatchlineinfo) (hksc_State *H, int match_line_info);
#endif /* HKSC_DECOMPILER */

/*
** dump functions
*/

/* print a function to stdout */
LUA_API void (lua_print) (hksc_State *H, int full);

/* dump one chunk; from ldump.c */
LUA_API int (lua_dump) (hksc_State *H, lua_Writer w, void *data);

#ifdef HKSC_DECOMPILER
/* decompile one chunk; from ldecomp.c */
LUA_API int (lua_decompile) (hksc_State *H, lua_Writer w, void *data);
#endif /* HKSC_DECOMPILER */

/*
** compatibility macros and functions
*/

#define lua_Chunkreader		lua_Reader
#define lua_Chunkwriter		lua_Writer



/******************************************************************************
* Copyright (C) 1994-2006 Lua.org, PUC-Rio.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/


#endif
