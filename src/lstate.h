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
#include "lvec.h"
#include "lzio.h"



struct lua_longjmp;  /* defined in ldo.c */



typedef struct stringtable {
  GCObject **hash;
  lu_int32 nuse;  /* number of elements */
  int size;
} stringtable;


typedef struct FilePrefixMap {
  TString *from, *to;
} FilePrefixMap;


#define TM_INDEX  0
#define TM_NEWINDEX  1

#if HKSC_STRUCTURE_EXTENSION_ON
#define MIN_PROTO_LIST_SIZE  16


typedef struct StructProtoList {
  StructProto **list;
  short nuse;  /* number of defined structure prototypes */
  short size;
} StructProtoList;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


/*
** `global state', shared by all threads of this state
*/
typedef struct global_State {
  int mode; /* input file mode, source of binary */
  int bytecode_endianness; /* bytecode endianness */
#ifdef HKSC_MULTIPLAT
  int target_plat;
  int target_ws;
#endif /* HKSC_MULTIPLAT */
  hksc_CompilerSettings settings; /* compiler/decompiler settings */
  stringtable strt;  /* hash table for strings */
#if HKSC_STRUCTURE_EXTENSION_ON
  StructProtoList protolist;  /* list of defined structure prototypes */
  Table *prototable;  /* hashtable for searching structure prototypes by name */
#endif
  lua_Alloc frealloc;  /* function to reallocate memory */
  void *ud;         /* auxiliary data to `frealloc' */
  lu_byte currentwhite;
  lu_byte midcycle;  /* (used in GC) true if in the middle of a cycle */
  lu_byte incyclecallback;
  GCObject *rootgc;  /* list of all collectable objects */
  Mbuffer buff;  /* temporary buffer for string concatentation */
  lu_mem GCthreshold;
  lu_mem totalbytes;  /* number of bytes currently allocated */
  VEC_DECL(FilePrefixMap, prefixmaps);
  const char *prefix_map_from;  /* OLD value in file prefix map */
  const char *prefix_map_to;  /* NEW value in file prefix map */
  lua_CFunction panic;  /* to be called in unprotected errors */
#if defined(LUA_CODT6)
  LoadStateCB debugLoadStateOpen; /* (COD) debug reader initializer */
  LoadStateCB debugLoadStateClose; /* (COD) debug reader finalizer */
#endif /* LUA_CODT6 */
  hksc_CycleCallback startcycle, endcycle;
  struct hksc_State *mainthread;
  /* only `__index' and `__newindex' are needed by the compiler */
  TString *tm_names[2];
  /* cached type names used by the compiler */
  TString *typenames[LUA_NUM_TYPE_OBJECTS+1];
#if HKSC_STRUCTURE_EXTENSION_ON
  /* cached reserved structure slot names */
  TString *slotnames[NUM_SLOTS_RESERVED+1];
#endif
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
#if defined(LUA_CODT6)
  const char *currdebugfile;
#endif /* LUA_CODT6 */
  const char *currinputname;
};


#define G(H)	((H)->h_G)
#define Settings(H) (G(H)->settings)

#define hksc_mode(H) (G(H)->mode)

#define hksc_seterror(H,s) ((H)->errormsg = (s))

/* macros for setting compiler options */
#define hksc_getemitstruct(H) (Settings(H).emit_struct)
#define hksc_setemitstruct(H,v) (Settings(H).emit_struct = (v))
#define hksc_getintliteralsenabled(H) (Settings(H).enable_int_literals)
#define hksc_setintliteralsenabled(H,v) (Settings(H).enable_int_literals = (v))
#define hksc_getbytecodestrippinglevel(H) (Settings(H).strip)
#define hksc_setbytecodestrippinglevel(H,v) (Settings(H).strip = (v))
#define hksc_getignoredebug(H) (Settings(H).ignore_debug)
#define hksc_setignoredebug(H,v) (Settings(H).ignore_debug = (v))
#ifdef HKSC_DECOMPILER
#define hksc_getmatchlineinfo(H) (Settings(H).match_line_info)
#define hksc_setmatchlineinfo(H,v) (Settings(H).match_line_info = (v))
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
  struct TypeAnalyzer ta;
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
#define gco2ta(o)  check_exp((o)->gch.tt == LUA_TTYPEANALYZER, &((o)->ta))
#define gco2uv(o)	check_exp((o)->gch.tt == LUA_TUPVAL, &((o)->uv))
#define ngcotouv(o) \
	check_exp((o) == NULL || (o)->gch.tt == LUA_TUPVAL, &((o)->uv))
#define gco2th(o)	check_exp((o)->gch.tt == LUA_TTHREAD, &((o)->th))

/* macro to convert any Lua object into a GCObject */
#define obj2gco(v)	(cast(GCObject *, (v)))

#endif

