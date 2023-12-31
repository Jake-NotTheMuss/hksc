/*
** $Id: lstate.c $
** Global State
** See Copyright Notice in lua.h
*/


#include <stddef.h>

#define lstate_c
#define LUA_CORE

#include "hksclua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "llex.h"
#include "lmem.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"


#define state_size(x)	(sizeof(x) + LUAI_EXTRASPACE)
#define fromstate(l)	(cast(lu_byte *, (l)) - LUAI_EXTRASPACE)
#define tostate(l)   (cast(hksc_State *, cast(lu_byte *, l) + LUAI_EXTRASPACE))


/*
** Main thread combines a thread state and the global state
*/
typedef struct LG {
  hksc_State l;
  global_State g;
} LG;


#define createfixedstring(v,s) (v) = luaS_newliteral(H,s); luaS_fix(v)

/*
** open parts that may cause memory-allocation errors
*/
static void f_luaopen (hksc_State *H, void *ud) {
  global_State *g = G(H);
  UNUSED(ud);
#if HKSC_STRUCTURE_EXTENSION_ON
  g->prototable = luaH_new(H, 0, MIN_PROTO_LIST_SIZE);
  l_setbit(g->prototable->marked, FIXEDBIT);
  g->protolist.list = luaM_newvector(H, MIN_PROTO_LIST_SIZE, StructProto *);
  g->protolist.nuse = 0;
  g->protolist.size = MIN_PROTO_LIST_SIZE;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  luaS_resize(H, MINSTRTABSIZE);  /* initial size of string table */
  createfixedstring(g->tm_names[TM_INDEX], "__index");
  createfixedstring(g->tm_names[TM_NEWINDEX], "__newindex");
#if HKSC_STRUCTURE_EXTENSION_ON
  g->slotnames[SLOT_RESERVED_NONE] = NULL;
  createfixedstring(g->slotnames[SLOT_RESERVED_META], "__structmeta");
  createfixedstring(g->slotnames[SLOT_RESERVED_PROXY], "__structbacking");
  createfixedstring(g->slotnames[SLOT_RESERVED_INTERNAL], "__testdummy");
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  luaX_init(H);
  luaS_fix(luaS_newliteral(H, MEMERRMSG));
  luaS_fix(luaS_newliteral(H, MAINCHUNKNAME));
  g->GCthreshold = 4*g->totalbytes;
}


static void preinit_state (hksc_State *H, global_State *g) {
  G(H) = g;
  H->errorJmp = NULL;
  H->nCcalls = 0;
  H->status = 0;
  H->errormsg = NULL;
  H->last_result = NULL;
#if defined(LUA_CODT6)
  H->currdebugfile = NULL;
#endif /* defined(LUA_CODT6) */
  H->currinputname = NULL;
}


static void close_state (hksc_State *H) {
  global_State *g = G(H);
  luaC_freeall(H);  /* collect all objects */
  lua_assert(g->rootgc == obj2gco(H));
  lua_assert(g->strt.nuse == 0);
  luaM_freearray(H, G(H)->strt.hash, G(H)->strt.size, TString *);
  luaM_freearray(H, g->prefixmaps.array, g->prefixmaps.size, fileprefixmap);
#if HKSC_STRUCTURE_EXTENSION_ON
  luaM_freearray(H, g->protolist.list, g->protolist.size, StructProto *);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  luaZ_freebuffer(H, &g->buff);
  lua_assert(g->totalbytes == sizeof(LG));
  (*g->frealloc)(g->ud, fromstate(H), state_size(LG), 0);
}


LUA_API hksc_State *lua_newstate (hksc_StateSettings *settings) {
  lua_Alloc f;
  void *ud;
  int i;
  hksc_State *H;
  global_State *g;
  void *h;
  if (settings == NULL || settings->frealloc == NULL) return NULL;
  f = settings->frealloc;
  ud = settings->ud;
  h = (*f)(ud, NULL, 0, state_size(LG));
  (void)i;
  if (h == NULL) return NULL;
  H = tostate(h);
  g = &((LG *)H)->g;
  H->next = NULL;
  H->tt = LUA_TTHREAD;
  g->currentwhite = bitmask(FIXEDBIT);
  H->marked = luaC_white(g);
  set2bits(H->marked, FIXEDBIT, SFIXEDBIT);
  preinit_state(H, g);
  g->frealloc = f;
  g->ud = ud;
  g->mainthread = H;
  g->strt.size = 0;
  g->strt.nuse = 0;
  g->strt.hash = NULL;
  g->mode = settings->mode;
  g->bytecode_endianness = settings->bytecode_endianness;
#ifdef HKSC_MULTIPLAT
  g->target_plat = settings->target_plat;
  g->target_ws = settings->target_ws;
#endif /* HKSC_MULTIPLAT */
  g->settings = settings->compilersettings;
  luaZ_initbuffer(H, &g->buff);
  g->prefix_map_from = NULL;
  g->prefix_map_to = NULL;
  g->panic = NULL;
  g->midcycle = 0;
#ifdef LUA_DEBUG
  g->incyclecallback = 0;
#endif /* LUA_DEBUG */
  g->rootgc = obj2gco(H);
  g->totalbytes = sizeof(LG);
  g->prefixmaps.nuse = 0;
  g->prefixmaps.size = 0;
  g->prefixmaps.array = NULL;
  g->startcycle = g->endcycle = NULL;
#if defined(LUA_CODT6)
  g->debugLoadStateOpen = NULL;
  g->debugLoadStateClose = NULL;
#endif /* defined(LUA_CODT6) && defined(HKSC_DECOMPILER) */
#if HKSC_STRUCTURE_EXTENSION_ON
  g->protolist.list = NULL;
  g->protolist.nuse = g->protolist.size = 0;
  g->prototable = NULL;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  if (luaD_rawrunprotected(H, f_luaopen, NULL) != 0) {
    /* memory allocation error: free partial state */
    close_state(H);
    H = NULL;
  }
  else
    luai_userstateopen(H);
  return H;
}

LUA_API void lua_close (hksc_State *H) {
  H = G(H)->mainthread;  /* only the main thread can be closed */
  lua_lock(H);
  luai_userstateclose(H);
  close_state(H);
}

