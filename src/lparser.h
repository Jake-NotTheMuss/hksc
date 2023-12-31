/*
** $Id: lparser.h $
** Lua Parser
** See Copyright Notice in lua.h
*/

#ifndef lparser_h
#define lparser_h

#include "llimits.h"
#include "lobject.h"
#include "lzio.h"


/*
** Expression descriptor
*/

typedef enum {
  VVOID,	/* no value */
  VNIL,
  VTRUE,
  VFALSE,
  VK,		/* info = index of constant in `k' */
  VKNUM,	/* nval = numerical value */
  VLOCAL,	/* info = local register */
  VUPVAL,       /* info = index of upvalue in `upvalues' */
  VGLOBAL,	/* info = index of table; aux = index of global name in `k' */
  VSLOT,  /* info = struct register */
  VINDEXED,	/* info = table register; aux = index register (or `k') */
  VJMP,		/* info = instruction pc */
  VRELOCABLE,	/* info = instruction pc */
  VNONRELOC,	/* info = result register */
  VCALL,	/* info = instruction pc */
  VVARARG,	/* info = instruction pc */
  VINTRINSIC  /* ??? */
} expkind;


#define exptype(e) ((e)->inferred_type)
#define expproto(e)  ((e)->inferred_proto)

#define expisnil(e)  (exptype(e) == LUA_TNIL)
#define expisboolean(e)  (exptype(e) == LUA_TBOOLEAN)
#define expislud(e)  (exptype(e) == LUA_TLIGHTUSERDATA)
#define expisnumber(e)  (exptype(e) == LUA_TNUMBER)
#define expisstring(e)  (exptype(e) == LUA_TSTRING)
#define expistable(e)  (exptype(e) == LUA_TTABLE)
#define expisfunction(e)  (exptype(e) == LUA_TFUNCTION || \
                          expisifunction(e) || expiscfunction(e))
#define expisuserdata(e)  (exptype(e) == LUA_TUSERDATA)
#define expisthread(e)  (exptype(e) == LUA_TTHREAD)
#define expisifunction(e)  (exptype(e) == LUA_TIFUNCTION)
#define expiscfunction(e)  (exptype(e) == LUA_TCFUNCTION)
#define expisui64(e)  (exptype(e) == LUA_TUI64)
#define expisstruct(e)  (exptype(e) == LUA_TSTRUCT)

typedef struct expdesc {
  expkind k;
  union {
    struct {
      int info, aux;
      /* data for VSLOT expressions */
      struct StructProto *proto;  /* indexed structure */
      TValue structindex;  /* index object */
    } s;
    lua_Number nval;
    lu_int64 lval;
  } u;
  int t;  /* patch list of `exit when true' */
  int f;  /* patch list of `exit when false' */
  int inferred_type;
  struct StructProto *inferred_proto;
} expdesc;


typedef struct upvaldesc {
  lu_byte k;
  lu_byte info;
} upvaldesc;


struct BlockCnt;  /* defined in lparser.c */


/* state needed to generate code for a given function */
typedef struct FuncState {
  Proto *f;  /* current function header */
  Table *h;  /* table to find (and reuse) elements in `k' */
  TypeAnalyzer *a;
  struct FuncState *prev;  /* enclosing function */
  struct LexState *ls;  /* lexical state */
  struct hksc_State *H;  /* copy of the Lua state */
  struct BlockCnt *bl;  /* chain of current blocks */
  int pc;  /* next position to code (equivalent to `ncode') */
  int lasttarget;   /* `pc' of last `jump target' */
  int jpc;  /* list of pending jumps to `pc' */
  int freereg;  /* first free register */
  int nk;  /* number of elements in `k' */
  int np;  /* number of elements in `p' */
  short nlocvars;  /* number of elements in `locvars' */
  short nlocalslhs;  /* number of elements in `lhstyping' */
  lu_byte nactvar;  /* number of active local variables */
  upvaldesc upvalues[LUAI_MAXUPVALUES];  /* upvalues */
  unsigned short actvar[LUAI_MAXVARS];  /* declared-variable stack */
} FuncState;


LUAI_FUNC Proto *luaY_parser (hksc_State *H, ZIO *z, Mbuffer *buff,
                                            const char *name);
LUAI_FUNC void luaY_freetypeanalyzer (hksc_State *H, TypeAnalyzer *a);


#endif
