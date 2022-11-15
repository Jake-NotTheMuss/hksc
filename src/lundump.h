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
LUAI_FUNC Proto *luaU_undump (hksc_State *H, ZIO *Z, Mbuffer *buff,
                              const char *name);

/* make header; from lundump.c */
LUAI_FUNC void luaU_header (char *h, int endianswap);

/* dump one chunk; from ldump.c */
LUAI_FUNC int luaU_dump (hksc_State *H,
                         const Proto *f, lua_Writer w, void *data);

#ifdef luac_c
/* print one chunk; from print.c */
LUAI_FUNC void luaU_print (const Proto *f, int full);
#endif

/* for header of binary files -- this is Lua 5.1 */
#define LUAC_VERSION		0x51

/* for header of binary files -- this is the official format */
#define LUAC_FORMAT		14

/* size of header of binary files */
#define LUAC_HEADERSIZE		sizeof(HkscHeader)

/* this is an offline compiler - bytecode is not shared */
#define LUAC_SHARED_BYTECODE 0

typedef struct HkscHeader {
  char signature[sizeof(LUA_SIGNATURE)-1]; /* Lua binary signature */
  char version;     /* Lua version */
  char format;      /* Lua format */
  char endianswap;  /* true if need to swap endianness when loading/dumping */
  char sizeint;     /* size of int */
  char sizesize;    /* size of size_t */
  char sizeinstr;   /* size of Instruction */
  char sizenumber;  /* size of lua_Number */
  char numberisint; /* true if lua_Number is integral */
  char compatmask;  /* compatibility flags */
  char sharedstate; /* true if compiled in a shared state */
} HkscHeader;

/* number of types in header of binary files */
#define LUAC_NUMTYPES (LUA_NUM_TYPE_OBJECTS-1)


/* bytecode stripping levels */
#define BYTECODE_STRIPPING_NONE 0
#define BYTECODE_STRIPPING_PROFILING 1
#define BYTECODE_STRIPPING_ALL 2
#ifdef LUA_COD /* Cod extensions */
#define BYTECODE_STRIPPING_DEBUG_ONLY 3
#define BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION 4
#endif /* LUA_COD */

/* TODO: These are compatibility bits
  HKS_GETGLOBAL_MEMOIZATION
  HKS_STRUCTURE_EXTENSION_ON
  HKS_SELF
  HKS_WITHDOUBLES
  HKS_WITHNATIVEINT
  -----
  HKS_COMPATIBILITY_BIT_MEMOIZATION
  HKS_COMPATIBILITY_BIT_STRUCTURES
  HKS_COMPATIBILITY_BIT_SELF
  HKS_COMPATIBILITY_BIT_DOUBLES
  HKS_COMPATIBILITY_BIT_NATIVEINT
Note: Civ6 settings are 0xb == 01011:
  MEMOIZATION:  ON
  STRUCTURES:   ON
  SELF:         OFF
  DOUBLES:      ON
  NATIVEINT:    OFF
Note: CoD settings are 0:
  MEMOIZATION:  OFF
  STRUCTURES:   OFF
  SELF:         OFF
  DOUBLES:      OFF
  NATIVEINT:    OFF
*/


#define aligned2type(x,size) (((x) + ((size)-1)) & ~((size)-1))
#define aligned2nativetype(x,t) aligned2type(x,sizeof(t))

#define aligned2int(x)     aligned2type(x,HKSC_SIZE_INT)
#define aligned2size(x)    aligned2type(x,HKSC_SIZE_SIZE)
#define aligned2instr(x)   aligned2type(x,HKSC_SIZE_INSTR)
#define aligned2number(x)  aligned2type(x,HKSC_SIZE_NUMBER)

#ifndef LUA_MULTIPLAT_COMPAT /* use native host sizes */
# define HKSC_SIZE_INT     sizeof(int)
# define HKSC_SIZE_SIZE    sizeof(size_t)
# define HKSC_SIZE_INSTR   sizeof(Instruction)
# define HKSC_SIZE_NUMBER  sizeof(lua_Number)
#else
/* TODO: use explicit sizes from dump settings */
#error "Unimplemented"
#endif

/* for data alignment in header */
#define ALIGN(p,n) (void *)(((long)(p) + (n)-1) & ~((n)-1))
#define LUAC_PADCHAR 0x5f /* TODO: is this supposed to be '_'? in source */

#endif
