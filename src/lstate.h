/*
** $Id: lstate.h,v 2.23 2005/07/09 13:22:34 roberto Exp roberto $
** Global State
** See Copyright Notice in lua.h
*/

#ifndef lstate_h
#define lstate_h

#include "lua.h"

#include "lobject.h"
#include "lzio.h"




struct lua_longjmp;  /* defined in ldo.c */



typedef struct stringtable {
  GCObject **hash;
  lu_int32 nuse;  /* number of elements */
  int size;
} stringtable;




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

/*typedef int (*lua_LineMap)(const char *, int);*/

/*
** global settings shared by all states
*/
typedef struct hksc_Settings
{
  /* general settings */
  lu_byte sharing_format; /* bytecode sharing format, default = INPLACE */
  lu_byte sharing_mode; /* bytecode sharing mode, default = ON */

  /* compiler-specific settings */
  lu_byte emit_struct; /* whether `hstructure' and `hmake' should be emitted */
  lu_byte enable_int_literals; /* int literal setting */
  lu_byte strip; /* bytecode stripping level */
  const char **strip_names;
  /*lua_LineMap debug_map;*/

  /* decompiler-specific settings */
  lu_byte ignore_debug; /* ignore debug info if present */
  lu_byte match_line_info; /* emit statements according to the line mapping */
} hksc_Settings;

/*
** hksc modes
*/
#define HKSC_MODE_DEFAULT   0 /* infer from content of first file */
#define HKSC_MODE_COMPILE   1 /* compiling source */
#define HKSC_MODE_DECOMPILE 2 /* decompiling bytecode */

/*
** `global state', shared by all threads of this state
*/
typedef struct global_State {
  int mode; /* compiling or decompiling? */
  int endianness; /* bytecode endianness */
  hksc_Settings settings; /* compiler/decompiler settings */
  stringtable strt;  /* hash table for strings */
  lua_Alloc frealloc;  /* function to reallocate memory */
  void *ud;         /* auxiliary data to `frealloc' */
  lu_byte currentwhite;
  lu_byte gcstate;  /* state of garbage collector */
  int sweepstrgc;  /* position of sweep in `strt' */
  GCObject *rootgc;  /* list of all collectable objects */
  GCObject **sweepgc;  /* position of sweep in `rootgc' */
  Mbuffer buff;  /* temporary buffer for string concatentation */
  lu_mem totalbytes;  /* number of bytes currently allocated */
  lu_mem estimate;  /* an estimate of number of bytes actually in use */
  lu_mem gcdept;  /* how much GC is `behind schedule' */
  int gcstepmul;  /* GC `granularity' */
  lua_CFunction panic;  /* to be called in unprotected errors */
#if 0
  hksc_LogFunction log;  /* callback for logging debug messages */
#endif
  struct hksc_State *mainthread;
} global_State;

/*
** `per thread' state
*/
struct hksc_State {
  CommonHeader;
  lu_byte status;
  global_State *h_G;
  Proto *last_result;
  unsigned short nCcalls;  /* number of nested C calls */
  struct lua_longjmp *errorJmp;  /* current error recover point */
  const char *errormsg; /* the last error message */
};


#define G(H)	((H)->h_G)
#define Settings(H) (G(H)->settings)


/* macros for getting/setting the error message of an hksc_State */
#define hksc_luaE_geterrormsg(H) ((H)->errormsg)
#define hksc_luaE_seterrormsg(H,s) ((H)->errormsg = (s))
#define luaE_geterrormsg(H) hksc_luaE_geterrormsg(H)
#define luaE_seterrormsg(H,s) hksc_luaE_seterrormsg(H,s)

#define hksc_luaE_mode(H) (G(H)->mode)
#define luaE_mode(H) hksc_luaE_mode(H)

/* macros for setting compiler options */
#define hksc_setEmitStruct(H,v) (Settings(H).emit_struct = (v))
#define hksc_setIntLiteralsEnabled(H,v) (Settings(H).enable_int_literals = (v))
#define hksc_setBytecodeStrippingLevel(H,v) (Settings(H).strip = (v))
#define hksc_setIgnoreDebug(H,v) (Settings(H).ignore_debug = (v))
#define hksc_setMatchLineInfo(H,v) (Settings(H).match_line_info = (v))

#define hksc_getSharingFmt(H) (Settings(H).sharing_format)
#define hksc_getSharingMode(H) (Settings(H).sharing_mode)
#define hksc_getEmitStruct(H) (Settings(H).emit_struct)
#define hksc_getIntLiteralsEnabled(H) (Settings(H).enable_int_literals)
#define hksc_getStrip(H) (Settings(H).strip)
#define hksc_getIgnoreDebug(H) (Settings(H).ignore_debug)
#define hksc_getMatchLineInfo(H) (Settings(H).match_line_info)

#define hksc_setEmitStruct(H,v) (Settings(H).emit_struct = (v))


/*
** Union of all collectable objects
*/
union GCObject {
  GCheader gch;
  union TString ts;
  union Udata u;
  union Closure cl;
  struct Table h;
  struct Proto p;
  struct UpVal uv;
  struct hksc_State th;  /* thread */
};


/* macros to convert a GCObject into a specific value */
#define rawgco2ts(o)	check_exp((o)->gch.tt == LUA_TSTRING, &((o)->ts))
#define gco2ts(o)	(&rawgco2ts(o)->tsv)
#define rawgco2u(o)	check_exp((o)->gch.tt == LUA_TUSERDATA, &((o)->u))
#define gco2u(o)	(&rawgco2u(o)->uv)
#define gco2cl(o)	check_exp((o)->gch.tt == LUA_TFUNCTION, &((o)->cl))
#define gco2h(o)	check_exp((o)->gch.tt == LUA_TTABLE, &((o)->h))
#define gco2p(o)	check_exp((o)->gch.tt == LUA_TPROTO, &((o)->p))
#define gco2uv(o)	check_exp((o)->gch.tt == LUA_TUPVAL, &((o)->uv))
#define ngcotouv(o) \
	check_exp((o) == NULL || (o)->gch.tt == LUA_TUPVAL, &((o)->uv))
#define gco2th(o)	check_exp((o)->gch.tt == LUA_TTHREAD, &((o)->th))

/* macro to convert any Lua object into a GCObject */
#define obj2gco(v)	(cast(GCObject *, (v)))


LUAI_FUNC hksc_State *luaE_newthread (hksc_State *H);
LUAI_FUNC void luaE_freethread (hksc_State *H, hksc_State *H1);

LUAI_FUNC lua_CFunction hksc_atpanic (hksc_State *H, lua_CFunction panicf);

LUAI_FUNC hksc_State *hksc_newstate (lua_Alloc f, void *ud);
LUAI_FUNC void hksc_close (hksc_State *H);
LUAI_FUNC void luaE_clearerr (hksc_State *H);

#endif

