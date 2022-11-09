/*
** $Id: lundump.h,v 1.39 2005/11/01 17:04:55 lhf Exp lhf $
** load precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#ifndef lundump_h
#define lundump_h

#include "lua.h"

#include "lobject.h"
#include "lzio.h"

/* load one chunk; from lundump.c */
LUAI_FUNC Proto* luaU_undump (hksc_State *H, ZIO* Z, Mbuffer* buff,
                              const char* name);

/* make header; from lundump.c */
LUAI_FUNC void luaU_header (char* h, int endianswap);

/* dump one chunk; from ldump.c */
LUAI_FUNC int luaU_dump (hksc_State *H,
                         const Proto* f, lua_Writer w, void* data);

#ifdef luac_c
/* print one chunk; from print.c */
LUAI_FUNC void luaU_print (const Proto* f, int full);
#endif

/* for header of binary files -- this is Lua 5.1 */
#define LUAC_VERSION		0x51

/* for header of binary files -- this is the official format */
#define LUAC_FORMAT		14

/* size of header of binary files */
#define LUAC_HEADERSIZE		14

/* number of types in header of binary files */
#define LUAC_NUMTYPES (LUA_TSTRUCT+1)


/* bytecode stripping levels */
#define BYTECODE_STRIPPING_NONE 0
#define BYTECODE_STRIPPING_PROFILING 1
#define BYTECODE_STRIPPING_ALL 2
#define BYTECODE_STRIPPING_DEBUG_ONLY 3
#define BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION 4

/*
** macros for testing stripping level properties
*/
#define needsfuncinfo(D) ((D)->strip == BYTECODE_STRIPPING_NONE || \
  (D)->strip == BYTECODE_STRIPPING_PROFILING || \
  (D)->strip == BYTECODE_STRIPPING_ALL)

#define needsdebuginfo(D) ((D)->strip == BYTECODE_STRIPPING_NONE || \
  (D)->strip == BYTECODE_STRIPPING_DEBUG_ONLY)

#define aligned2instr(x) \
  (((x) + (HKSC_SIZE_INSTR-1)) & ~(HKSC_SIZE_INSTR-1))

#ifdef CODT7_COMPAT
/* hardcoded to match cod */
# define HKSC_SIZE_INT    4
# define HKSC_SIZE_SIZE   8
# define HKSC_SIZE_INSTR  4
# define HKSC_SIZE_NUMBER 4
#else /* !CODT7_COMPAT */
# define HKSC_SIZE_INT    sizeof(int)
# define HKSC_SIZE_SIZE   sizeof(size_t)
# define HKSC_SIZE_INSTR  sizeof(Instruction)
# define HKSC_SIZE_NUMBER sizeof(lua_Number)
#endif /* CODT7_COMPAT */

/* for data alignment in header */
#define ALIGN(p,n) (void *)(((long)(p) + (n)-1) & ~((n)-1))
#define LUAC_PADCHAR 0x5f /* TODO: is this supposed to be '_'? in source */

#endif
