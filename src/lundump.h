/*
** $Id: lundump.h,v 1.39 2005/11/01 17:04:55 lhf Exp lhf $
** load precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#ifndef lundump_h
#define lundump_h

#include "hksc_begin_code.h"

#include "lobject.h"
#include "lzio.h"

/* load one chunk; from lundump.c */
/*LUAI_FUNC Proto* luaU_undump (ZIO* Z, Mbuffer* buff, const char* name);*/

/* make header; from lundump.c */
LUAI_FUNC void luaU_header (char* h);

/* dump one chunk; from ldump.c */
LUAI_FUNC int luaU_dump (hksc_State *H,
                         const Proto* f, lua_Writer w, void* data, int strip);

#ifdef luac_c
/* print one chunk; from print.c */
/*LUAI_FUNC void luaU_print (const Proto* f, int full);*/
#endif

/* for header of binary files -- this is Lua 5.1 */
#define LUAC_VERSION		0x51

/* for header of binary files -- this is the official format */
#define LUAC_FORMAT		0

/* size of header of binary files */
#define LUAC_HEADERSIZE		238

#define BYTECODE_STRIPPING_NONE 0
#define BYTECODE_STRIPPING_PROFILING 1
#define BYTECODE_STRIPPING_ALL 2
#define BYTECODE_STRIPPING_DEBUG_ONLY 3
#define BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION 4

/* for data alignment in header */
#define ALIGN(p,n) (void *)(((long)(p) + (n)-1) & ~((n)-1))

#endif
