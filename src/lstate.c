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


/*
** open parts that may cause memory-allocation errors
*/
static void f_luaopen (hksc_State *H, void *ud) {
  global_State *g = G(H);
  UNUSED(ud);
  luaS_resize(H, MINSTRTABSIZE);  /* initial size of string table */
  luaX_init(H);
  luaS_fix(luaS_newliteral(H, MEMERRMSG));
  luaS_fix(luaS_newliteral(H, MAINCHUNKNAME));
  (void)g;
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
  g->startcycle = g->endcycle = NULL;
#if defined(LUA_CODT6)
  g->debugLoadStateOpen = NULL;
  g->debugLoadStateClose = NULL;
#endif /* defined(LUA_CODT6) && defined(HKSC_DECOMPILER) */
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

