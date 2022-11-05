/*
** $Id: lstate.c,v 2.34 2005/09/20 17:55:53 roberto Exp roberto $
** Global State
** See Copyright Notice in lua.h
*/


#include <stddef.h>

#define lstate_c
#define LUA_CORE
#include "hksc_begin_code.h"

#include "lua.h"

#include "ldebug.h"
#include "lerror.h"
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
  /*g->GCthreshold = 4*g->totalbytes;*/
}


static void preinit_state (hksc_State *H, global_State *g) {
  G(H) = g;
  H->errorJmp = NULL;
  H->nCcalls = 0;
  H->status = 0;
  H->errormsg = NULL;
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
  settings->endian = HKSC_DEFAULT_ENDIAN;
  settings->sharing_format = HKSC_BYTECODE_DEFAULT;
  settings->sharing_mode = HKSC_SHARING_MODE_OFF;
  /* compiler settings */
  settings->emit_struct = 0;
  settings->literals = INT_LITERALS_LUD | INT_LITERALS_UI64;
  settings->strip_names = NULL;
  /* decompiler settings */
  settings->ignore_debug = 0;
  settings->match_line_info = 1;
}


hksc_State *luaE_newthread (hksc_State *H) {
#if 0
  hksc_State *H1 = tostate(luaM_malloc(H, state_size(hksc_State)));
  luaC_link(H, obj2gco(H1), LUA_TTHREAD);
  preinit_state(H1, G(H));
  stack_init(H1, H);  /* init stack */
  setobj2n(H, gt(H1), gt(H));  /* share table of globals */
  H1->hookmask = H->hookmask;
  H1->basehookcount = H->basehookcount;
  H1->hook = H->hook;
  resethookcount(H1);
  lua_assert(iswhite(obj2gco(H1)));
  return H1;
#endif
  return NULL;
}


void luaE_freethread (hksc_State *H, hksc_State *H1) {
#if 0
  luaF_close(H1, H1->stack);  /* close all upvalues for this thread */
  lua_assert(H1->openupval == NULL);
  luai_userstatefree(H1);
  freestack(H, H1);
  luaM_freemem(H, fromstate(H1), state_size(hksc_State));
#endif
}

lua_CFunction hksc_atpanic (hksc_State *H, lua_CFunction panicf) {
  lua_CFunction old;
  /*lua_lock(H);*/
  old = G(H)->panic;
  G(H)->panic = panicf;
  /*lua_unlock(H);*/
  return old;
}


hksc_State *hksc_newstate (lua_Alloc f, void *ud) {
  int i;
  hksc_State *H;
  global_State *g;
  void *h = (*f)(ud, NULL, 0, state_size(LG));
  if (h == NULL) return NULL;
  H = tostate(h);
  g = &((LG *)H)->g;
  H->tt = LUA_TTHREAD;
  g->currentwhite = bit2mask(LIVEBIT, FIXEDBIT);
  H->marked = luaC_live(g);
  set2bits(H->marked, FIXEDBIT, SFIXEDBIT);
  preinit_state(H, g);
  g->frealloc = f;
  g->ud = ud;
  g->mainthread = H;
  g->strt.size = 0;
  g->strt.nuse = 0;
  g->strt.hash = NULL;
  g->mode = HKSC_MODE_DEFAULT;
  hksc_default_settings(&g->settings);
  luaZ_initbuffer(H, &g->buff);
  g->panic = NULL;
  g->gcstate = GCSpause;
  /*g->rootgc = obj2gco(H);*/
  g->rootgc = NULL;
  g->sweepstrgc = 0;
  g->sweepgc = &g->rootgc;
  g->totalbytes = sizeof(LG);
  if (luaD_rawrunprotected(H, f_luaopen, NULL) != 0) {
    /* memory allocation error: free partial state */
    close_state(H);
    H = NULL;
  }
  return H;
}

void hksc_close (hksc_State *H) {
  H = G(H)->mainthread;  /* only the main thread can be closed */
  /*luai_userstateclose(H);*/
  /*lua_lock(H);*/
  close_state(H);
}

