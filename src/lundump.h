/*
** $Id: lundump.h $
** load precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#ifndef lundump_h
#define lundump_h

#include "lobject.h"
#include "lzio.h"


/* define static functions needed for both dumping and loading */
#if defined(ldump_c) || defined(lundump_c)

/* swap endianness of a sequence of bytes if needed */
#define correctendianness(S,x) \
  if ((S)->swapendian) swapvarendianness(x)

#define swapvarendianness(x) swapendianness(&(x), sizeof(x))

static void swapendianness(void *p, size_t n) {
  size_t i = 0;
  size_t n2 = n/2;
  char *x = cast(char *, p);
  while (n-- > n2) {
    char t = x[i];
    x[i] = x[n];
    x[n] = t;
    i++;
  }
}

static int isbigendian() {
  int x=1;
  return ((char)*(char *)&x == 0);
}

struct target_info {
  int sizeint, sizesize, sizeinstr;
  int needendianswap;
};

LUAI_FUNC void luaU_target_info (hksc_State *H, struct target_info *target);

#endif /* ldump_c || lundump_c */

#if defined(LUA_CODT6)

/*
** Callback function signature for constructing/destructing a debug load state.
** The LoadState structure is private to lundump.c; the individual members that
** need initializing are passed by reference (ZIO and Mbuffer), as well as the
** name of the chunk. The constructor/destructor are referenced with separate
** pointers in the global state, and it is ensured that the destructor will
** fire if it is non-NULL and if the constructor was fired even if an exception
** is thrown in between the 2 callbacks, eliminiating the library's chance of
** leaking user memory.
*/
typedef int (*LoadStateCB)(hksc_State *H, ZIO *z, Mbuffer *b,
                           const char *name);

typedef int (*DebugLoadCB) (hksc_State *H, const char *name);

#endif /* LUA_CODT6 */

/* load one chunk; from lundump.c */
LUAI_FUNC Proto *luaU_undump (hksc_State *H, ZIO *Z, Mbuffer *buff,
                              const char *name);

#if defined(ldump_c) || defined(lundump_c)
/* make header; from lundump.c */
LUAI_FUNC void luaU_header (struct target_info *target, char *h);
#endif /* ldump_c || lundump_c */

/* dump one chunk; from ldump.c */
LUAI_FUNC int luaU_dump (hksc_State *H,
                         const Proto *f, lua_Writer w, void *data);


#ifdef HKSC_DECOMPILER
/* decompile one chunk; from ldecomp.c */
LUAI_FUNC int luaU_decompile (hksc_State *H,
                         const Proto *f, lua_Writer w, void *data);
#endif /* HKSC_DECOMPILER */

/* print one chunk; from lprint.c */
LUAI_FUNC int luaU_print (hksc_State *H, const Proto *f, lua_Writer w,
                          void *data, int full);

/* for header of binary files -- this is Lua 5.1 */
#define LUAC_VERSION		0x51

/* for header of binary files -- this is the official format */
#if defined(LUA_CODT7)
#define LUAC_FORMAT  14 /* T7 format version */
#define HKSC_FORMAT_VERSION  14 /* T7 format version */
#elif defined(LUA_CODT6)
#define LUAC_FORMAT  13 /* T6 format version */
#define HKSC_FORMAT_VERSION  13 /* T6 format version */
#elif defined(HKSC_FORMAT_VERSION)
#define LUAC_FORMAT  HKSC_FORMAT_VERSION
#else
#define LUAC_FORMAT  14
#define HKSC_FORMAT_VERSION  14
/*#error "You need to define HKSC_FORMAT_VERSION"*/
#endif


/* define the byte offsets of Luac header fields */
enum LUAC_HEADER_FIELD {
  HDR_SIGNATURE = 0,  /* Lua binary signature */
  HDR_VERSION = sizeof(LUA_SIGNATURE)-1,  /* Lua version */
  HDR_FORMAT,  /* Lua format version */
  HDR_ENDIAN,  /* 0 if need to swap endianness when loading/dumping */
  HDR_SIZE_INT,  /* size of int */
  HDR_SIZE_SIZE,  /* size of size_t */
  HDR_SIZE_INST,  /* size of Instruction */
  HDR_SIZE_NUM,  /* size of lua_Number */
  HDR_INTEGRAL,  /* true if lua_Number is integral */
  HDR_COMPAT,  /* compatibility bits */
  HDR_SHARED,  /* true if compiled in a shared state */
  LUAC_HEADERSIZE  /* size of header of binary files */
};

/* the latest version of the HavokScript binary header is 14 bytes
   note: there are earlier format versions where the header size was smaller */
lua_static_assert(LUAC_HEADERSIZE == 14);


/* number of types in header of binary files */
#define LUAC_NUMTYPES (LUA_NUM_TYPE_OBJECTS-1)

/* maximum length of a reserved word */
#define DEFTYPE(t) char buf_##t[sizeof(#t)];
union max_type_length {
#include "ltype.def"
};
#undef DEFTYPE

#define MAX_TYPE_LENGTH (sizeof(union max_type_length)/sizeof(char))


/* stream position alignment in bytecode */
#define aligned2type(x,t) (((x) + (sizeof(t)-1)) & ~(sizeof(t)-1))
#define aligned2nativetype(x,t) aligned2type(x,t)

#define aligned2int(x)     aligned2type(x,int)
#define aligned2size(x)    aligned2type(x,size_t)
#define aligned2instr(x)   aligned2type(x,Instruction)
#define aligned2num(x)     aligned2type(x,lua_Number)


#endif
