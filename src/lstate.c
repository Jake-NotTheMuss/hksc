/*
** $Id: lstate.c,v 2.34 2005/09/20 17:55:53 roberto Exp roberto $
** Global State
** See Copyright Notice in lua.h
*/


#include <stddef.h>

#define lstate_c
#define LUA_CORE

#include "lua.h"

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
  luaS_mainchunk = luaS_newliteral(H, MAINCHUNKNAME);
  luaS_fix(luaS_mainchunk);
  (void)g;
  /*g->GCthreshold = 4*g->totalbytes;*/
}


static void preinit_state (hksc_State *H, global_State *g) {
  G(H) = g;
  H->errorJmp = NULL;
  H->nCcalls = 0;
  H->status = 0;
  H->errormsg = NULL;
  H->last_result = NULL;
#if defined(LUA_COD) && defined(HKSC_DECOMPILER)
  H->currdebugfile = NULL;
#endif /* defined(LUA_COD) && defined(HKSC_DECOMPILER) */
}


static void close_state (hksc_State *H) {
  global_State *g = G(H);
  luaC_freeall(H);  /* collect all objects */
  lua_assert(g->rootgc == NULL /*obj2gco(H)*/);
  lua_assert(g->strt.nuse == 0);
  luaM_freearray(H, G(H)->strt.hash, G(H)->strt.size, TString *);
  luaZ_freebuffer(H, &g->buff);
  lua_assert(g->totalbytes == sizeof(LG));
  (*g->frealloc)(g->ud, fromstate(H), state_size(LG), 0);
}


static void
hksc_default_settings(hksc_Settings *settings)
{
  /* general settings */
  settings->ignore_debug = 0;
  /* compiler settings */
  settings->emit_struct = 0;
  settings->enable_int_literals = INT_LITERALS_NONE;
  /*settings->strip_names = NULL;*/
#ifdef HKSC_DECOMPILER /* decompiler settings */
  settings->match_line_info = 1;
#endif /* HKSC_DECOMPILER */
}

#define SETCALLBACK_DECL(type, field, suffix) \
type hksc_##suffix (hksc_State *H, type f) { \
  type old; \
  lua_lock(H); \
  old = G(H)->field; \
  G(H)->field = f; \
  lua_unlock(H); \
  return old; \
}

SETCALLBACK_DECL(lua_CFunction, panic, atpanic)
SETCALLBACK_DECL(hksc_CycleCallback, startcycle, onstartcycle)
SETCALLBACK_DECL(hksc_CycleCallback, endcycle, onendcycle)

#undef SETCALLBACK_DECL

#if 0
lua_CFunction hksc_atpanic (hksc_State *H, lua_CFunction panicf) {
  lua_CFunction old;
  lua_lock(H);
  old = G(H)->panic;
  G(H)->panic = panicf;
  lua_unlock(H);
  return old;
}
#endif


#if 0
hksc_LogFunction hksc_logfunc (hksc_State *H, lua_LogFunction logf) {
  lua_LogFunction old;
  lua_lock(H);
  old = G(H)->log;
  G(H)->log = logf;
  lua_unlock(H);
  return old;
}
#endif

hksc_State *hksc_newstate (lua_Alloc f, void *ud) {
  int i;
  hksc_State *H;
  global_State *g;
  void *h = (*f)(ud, NULL, 0, state_size(LG));
  (void)i;
  if (h == NULL) return NULL;
  H = tostate(h);
  g = &((LG *)H)->g;
  H->tt = LUA_TTHREAD;
  g->currentwhite = bit2mask(LIVEBIT, FIXEDBIT);
  H->marked = luaC_white(g);
  set2bits(H->marked, FIXEDBIT, SFIXEDBIT);
  preinit_state(H, g);
  g->frealloc = f;
  g->ud = ud;
  g->mainthread = H;
  g->strt.size = 0;
  g->strt.nuse = 0;
  g->strt.hash = NULL;
  g->mode = HKSC_MODE_DEFAULT;
  g->endianness = HKSC_DEFAULT_ENDIAN;
  hksc_default_settings(&g->settings);
  luaZ_initbuffer(H, &g->buff);
  g->panic = NULL;
  g->gcstate = GCSpause;
  /*g->rootgc = obj2gco(H);*/
  g->rootgc = NULL;
  g->sweepstrgc = 0;
  g->sweepgc = &g->rootgc;
  g->totalbytes = sizeof(LG);
  g->startcycle = g->endcycle = NULL;
#if defined(LUA_COD) && defined(HKSC_DECOMPILER)
  g->debugLoadStateOpen = NULL;
  g->debugLoadStateClose = NULL;
#endif /* defined(LUA_COD) && defined(HKSC_DECOMPILER) */
  if (luaD_rawrunprotected(H, f_luaopen, NULL) != 0) {
    /* memory allocation error: free partial state */
    close_state(H);
    H = NULL;
  }
  else
    luai_userstateopen(H);
  return H;
}

void hksc_close (hksc_State *H) {
  H = G(H)->mainthread;  /* only the main thread can be closed */
  luai_userstateclose(H);
  lua_lock(H);
  close_state(H);
}


void luaE_clearerr (hksc_State *H) {
  H->status = 0;
  H->errormsg = NULL;
}

