/*
** $Id: lstate.h $
** Global State
** See Copyright Notice in lua.h
*/

#ifndef lstate_h
#define lstate_h

#include "hksclua.h"

#include "lobject.h"
#include "lundump.h"
#include "lzio.h"



struct lua_longjmp;  /* defined in ldo.c */



typedef struct stringtable {
  GCObject **hash;
  lu_int32 nuse;  /* number of elements */
  int size;
} stringtable;


/*
** `global state', shared by all threads of this state
*/
typedef struct global_State {
  int mode; /* input file mode, source of binary */
  int bytecode_endianness; /* bytecode endianness */
  hksc_CompilerSettings settings; /* compiler/decompiler settings */
  stringtable strt;  /* hash table for strings */
  lua_Alloc frealloc;  /* function to reallocate memory */
  void *ud;         /* auxiliary data to `frealloc' */
  lu_byte currentwhite;
  lu_byte gcstate;  /* state of garbage collector */
  int sweepstrgc;  /* position of sweep in `strt' */
  GCObject *rootgc;  /* list of all collectable objects */
  GCObject **sweepgc;  /* position of sweep in `rootgc' */
  Mbuffer buff;  /* temporary buffer for string concatentation */
  lu_mem GCthreshold;
  lu_mem totalbytes;  /* number of bytes currently allocated */
  lu_mem estimate;  /* an estimate of number of bytes actually in use */
  lu_mem gcdept;  /* how much GC is `behind schedule' */
  int gcpause;  /* size of pause between successive GCs */
  int gcstepmul;  /* GC `granularity' */
  const char *prefix_map_from;  /* OLD value in file prefix map */
  const char *prefix_map_to;  /* NEW value in file prefix map */
#ifdef HKSC_LOGGING
  hksc_LogContext logctx;
#endif
  lua_CFunction panic;  /* to be called in unprotected errors */
#if defined(LUA_COD)
  LoadStateCB debugLoadStateOpen; /* (COD) debug reader initializer */
  LoadStateCB debugLoadStateClose; /* (COD) debug reader finalizer */
#endif /* LUA_COD */
  hksc_CycleCallback startcycle, endcycle;
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
#if defined(LUA_COD)
  const char *currdebugfile;
#endif /* LUA_COD */
};


#define G(H)	((H)->h_G)
#define Settings(H) (G(H)->settings)

#define hksc_mode(H) (G(H)->mode)

#define hksc_seterror(H,s) ((H)->errormsg = (s))

/* macros for setting compiler options */
#define hksc_getEmitStruct(H) (Settings(H).emit_struct)
#define hksc_setEmitStruct(H,v) (Settings(H).emit_struct = (v))
#define hksc_getIntLiteralsEnabled(H) (Settings(H).enable_int_literals)
#define hksc_setIntLiteralsEnabled(H,v) (Settings(H).enable_int_literals = (v))
#define hksc_getBytecodeStrippingLevel(H) (Settings(H).strip)
#define hksc_setBytecodeStrippingLevel(H,v) (Settings(H).strip = (v))
#define hksc_getIgnoreDebug(H) (Settings(H).ignore_debug)
#define hksc_setIgnoreDebug(H,v) (Settings(H).ignore_debug = (v))
#ifdef HKSC_DECOMPILER
#define hksc_getMatchLineInfo(H) (Settings(H).match_line_info)
#define hksc_setMatchLineInfo(H,v) (Settings(H).match_line_info = (v))
#endif /* HKSC_DECOMPILER */


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
#ifdef HKSC_DECOMPILER
  struct Analyzer a;
#endif /* HKSC_DECOMPILER */
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
#define gco2p(o)  check_exp((o)->gch.tt == LUA_TPROTO, &((o)->p))
#ifdef HKSC_DECOMPILER
#define gco2a(o)	check_exp((o)->gch.tt == LUA_TANALYZER, &((o)->a))
#endif /* HKSC_DECOMPILER */
#define gco2uv(o)	check_exp((o)->gch.tt == LUA_TUPVAL, &((o)->uv))
#define ngcotouv(o) \
	check_exp((o) == NULL || (o)->gch.tt == LUA_TUPVAL, &((o)->uv))
#define gco2th(o)	check_exp((o)->gch.tt == LUA_TTHREAD, &((o)->th))

/* macro to convert any Lua object into a GCObject */
#define obj2gco(v)	(cast(GCObject *, (v)))

#endif

