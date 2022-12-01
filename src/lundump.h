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


/* define static functions needed by both lundump.c and ldump.c */
#if defined(lundump_c) || defined(ldump_c)

/* swap endianness of a sequence of bytes if needed */
#define correctendianness(s,x) \
  if ((s)->swapendian) swapendianness((char *)&x,sizeof(x))

static void swapendianness(char *x, size_t n) {
  size_t i = 0;
  while (n-- != 0) {
    char t = x[i];
    x[i] = x[n];
    x[n] = t;
    i++;
  }
}

#endif /* lundump_c || ldump_c */

#ifdef LUA_COD
#define LUA_MAXUDATABUFF (LUAL_BUFFERSIZE + sizeof(void *))
typedef int (*LoadStateCB)(hksc_State *H,ZIO **z,Mbuffer **b,char *udata_buff,
                           const char *name);
#endif /* LUA_COD */

/* load one chunk; from lundump.c */
LUAI_FUNC Proto *luaU_undump (hksc_State *H, ZIO *Z, Mbuffer *buff,
                              const char *name);

/* make header; from lundump.c */
LUAI_FUNC void luaU_header (char *h, int swapendian);

/* dump one chunk; from ldump.c */
LUAI_FUNC int luaU_dump (hksc_State *H,
                         const Proto *f, lua_Writer w, void *data);


#ifdef HKSC_DECOMPILER
/* decompile one chunk; from ldecomp.c */
LUAI_FUNC int luaU_decompile (hksc_State *H,
                         const Proto *f, lua_Writer w, void *data);
#endif /* HKSC_DECOMPILER */

#ifdef hksc_c
/* print one chunk; from print.c */
LUAI_FUNC void luaU_print (const Proto *f, int full);
#endif

/* for header of binary files -- this is Lua 5.1 */
#define LUAC_VERSION		0x51

/* for header of binary files -- this is the official format */
#define LUAC_FORMAT		14

/* size of header of binary files */
#define LUAC_HEADERSIZE		sizeof(HkscHeader)


typedef struct HkscHeader {
  char signature[sizeof(LUA_SIGNATURE)-1]; /* Lua binary signature */
  char version;     /* Lua version */
  char formatversion; /* Lua format version */
  char swapendian;  /* true if need to swap endianness when loading/dumping */
  char sizeint;     /* size of int */
  char sizesize;    /* size of size_t */
  char sizeinstr;   /* size of Instruction */
  char sizenumber;  /* size of lua_Number */
  char numberisint; /* true if lua_Number is integral */
  char compatbits;  /* compatibility bits */
  char shared;      /* true if compiled in a shared state */
} HkscHeader;

/* number of types in header of binary files */
#define LUAC_NUMTYPES (LUA_NUM_TYPE_OBJECTS-1)

/* maximum length of a reserved word */
#define DEFTYPE(t) char buf_##t[sizeof(#t)];
union max_type_length {
#include "ltype.def"
};
#undef DEFTYPE

#define MAX_TYPE_LENGTH (sizeof(union max_type_length)/sizeof(char))


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

#define hksc_compatbits \
  ((HKSC_GETGLOBAL_MEMOIZATION   << HKSC_COMPATIBILITY_BIT_MEMOIZATION) | \
  (HKSC_STRUCTURE_EXTENSION_ON   << HKSC_COMPATIBILITY_BIT_STRUCTURES)  | \
  (HKSC_SELF                     << HKSC_COMPATIBILITY_BIT_SELF)        | \
  (HKSC_WITHDOUBLES              << HKSC_COMPATIBILITY_BIT_DOUBLES)     | \
  (HKSC_WITHNATIVEINT            << HKSC_COMPATIBILITY_BIT_NATIVEINT))


/* stream position alignment in bytecode */
#define aligned2type(x,t) (((x) + (sizeof(t)-1)) & ~(sizeof(t)-1))
#define aligned2nativetype(x,t) aligned2type(x,t)

#define aligned2int(x)     aligned2type(x,int)
#define aligned2size(x)    aligned2type(x,size_t)
#define aligned2instr(x)   aligned2type(x,Instruction)
#define aligned2num(x)     aligned2type(x,lua_Number)


#endif
