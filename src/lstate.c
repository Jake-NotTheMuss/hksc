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
  l_setbit(g->prototable->marked, GC_FIXED);
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
  H->debugsource.reader = NULL;
  H->debugsource.ud = NULL;
  H->debugsource.preload = NULL;
  H->debugsource.postload = NULL;
  H->debugsource.mem.ptr = NULL;
  H->debugsource.mem.size = 0;
  H->debugsource.cycles = 0;
#endif /* defined(LUA_CODT6) */
  H->currinputname = NULL;
}


static void close_state (hksc_State *H) {
  global_State *g = G(H);
  luaC_freeall(H);  /* collect all objects */
  lua_assert(g->rootgc == NULL);
  lua_assert(g->strt.nuse == 0);
  luaM_freearray(H, G(H)->strt.hash, G(H)->strt.size, TString *);
  VEC_FREE(H, g->prefixmaps);
#if HKSC_STRUCTURE_EXTENSION_ON
  luaM_freearray(H, g->protolist.list, g->protolist.size, StructProto *);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  luaZ_freebuffer(H, &g->buff);
#ifdef LUA_CODT6
  luaE_allocdebugsource(H, 0);
#endif
  lua_assert(g->totalbytes == sizeof(LG));
  (*g->frealloc)(g->ud, fromstate(H), state_size(LG), 0);
}

#ifdef HKSC_TESTING
void luaE_cleanstate (hksc_State *H) {
  global_State *g = G(H);
#if HKSC_STRUCTURE_EXTENSION_ON
  g->protolist.nuse = 0;
  if (g->prototable) {
    Table *h = g->prototable;
    int i = h->sizearray;
    while (i--)
      setnilvalue(&h->array[i]);
    i = sizenode(h);
    while (i--) {
      Node *n = gnode(h, i);
      TValue *key = key2tval(n);
      TValue *val = gval(n);
      if (ttislightuserdata(key)) {
        Udata *u = rawuvalue(val);
        u->uv.marked = 0;
      }
      setnilvalue(key);
      setnilvalue(val);
    }
    luaC_collectgarbage(H);
  }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  UNUSED(g);
}
#endif


#ifdef LUA_CODT6
/* when the library supplies the debug info readers, it allocates memory for
   its userdata */
void *luaE_allocdebugsource (hksc_State *H, size_t size) {
  void *ptr;
  if (size == H->debugsource.mem.size)
    return H->debugsource.mem.ptr;
  if (size != 0 && size < H->debugsource.mem.size)
    return H->debugsource.mem.ptr;
  ptr= luaM_realloc_(H, H->debugsource.mem.ptr, H->debugsource.mem.size, size);
  H->debugsource.mem.ptr = ptr;
  H->debugsource.mem.size = size;
  return ptr;
}
#endif /* LUA_CODT6 */


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
  H->marked = 0;
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
  g->panic = NULL;
  g->midcycle = 0;
#ifdef LUA_DEBUG
  g->logfile = NULL;
  g->incyclecallback = 0;
#endif /* LUA_DEBUG */
  g->rootgc = NULL;
  g->totalbytes = sizeof(LG);
  VEC_INIT(g->prefixmaps);
  g->startcycle = g->endcycle = NULL;
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

