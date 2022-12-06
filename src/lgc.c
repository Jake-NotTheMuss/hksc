/*
** $Id: lgc.c $
** Garbage Collector
** See Copyright Notice in lua.h
*/

#include <string.h>

#define lgc_c
#define LUA_CORE

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"


#define GCSTEPSIZE  1024u
#define GCSWEEPMAX  40
#define GCSWEEPCOST 10


#define setthreshold(g)  (g->GCthreshold = (g->estimate/100) * g->gcpause)


static void freestring (hksc_State *H, GCObject *o) {
  lua_assert(o->gch.tt == LUA_TSTRING);
  G(H)->strt.nuse--;
  luaM_freemem(H, o, sizestring(gco2ts(o)));
}


static void freeobj (hksc_State *H, GCObject *o) {
  switch (o->gch.tt) {
    case LUA_TPROTO: luaF_freeproto(H, gco2p(o)); break;
    case LUA_TTABLE: luaH_free(H, gco2h(o)); break;
    case LUA_TTHREAD: {
      lua_assert(gco2th(o) != H && gco2th(o) != G(H)->mainthread);
      /*luaE_freethread(H, gco2th(o));*/
      break;
    }
    default: lua_assert(0);
  }
}


#define sweepwholelist(H,p) sweeptemplist(H,p,MAX_LUMEM)
#define sweepwholestringlist(H,p) sweepstringlist(H,p,MAX_LUMEM)

static GCObject **sweeptemplist (hksc_State *H, GCObject **p, lu_mem count) {
  GCObject *curr;
  global_State *g = G(H);
  while ((curr = *p) != NULL && count-- > 0) {
    lua_assert(istemp(g, curr));
    *p = curr->gch.next;
    if (curr == g->rootgc)  /* is the first element of the list? */
      g->rootgc = curr->gch.next;  /* adjust first */
    freeobj(H, curr);
  }
  return p;
}


static GCObject **sweepstringlist (hksc_State *H, GCObject **p, lu_mem count) {
  GCObject *curr;
  global_State *g = G(H);
  int deadmask = otherwhite(g);
  while ((curr = *p) != NULL && count-- > 0) {
    lua_assert(curr->gch.tt == LUA_TSTRING);
    /*printf("string: %s\n", getstr(gco2ts(curr)));*/
    if ((curr->gch.marked ^ bitmask(LIVEBIT)) & deadmask) {  /* not dead? */
      lua_assert(!isdead(g, curr) || testbit(curr->gch.marked, FIXEDBIT));
      makelive(curr);  /* make it white (for next cycle) */
      p = &curr->gch.next;
    }
    else {  /* must erase `curr' */
      lua_assert(isdead(g, curr) || deadmask == bitmask(SFIXEDBIT));
      *p = curr->gch.next;
      lua_assert(curr != g->rootgc);
      /*if (curr == g->rootgc)*/  /* is the first element of the list? */
        /*g->rootgc = curr->gch.next;*/  /* adjust first */
      freestring(H, curr);
    }
  }
  return p;
}


static void checkSizes (hksc_State *H) {
  global_State *g = G(H);
  /* check size of string hash */
  if (g->strt.nuse < cast(lu_int32, g->strt.size/4) &&
      g->strt.size > MINSTRTABSIZE*2)
    luaS_resize(H, g->strt.size/2);  /* table is too big */
  /* check size of buffer */
  if (luaZ_sizebuffer(&g->buff) > LUA_MINBUFFER*2) {  /* buffer too big? */
    size_t newsize = luaZ_sizebuffer(&g->buff) / 2;
    luaZ_resizebuffer(H, &g->buff, newsize);
  }
}


void luaC_freeall (hksc_State *H) {
  global_State *g = G(H);
  int i;
  /* mask to collect all elements */
  g->currentwhite = bit2mask(LIVEBIT, SFIXEDBIT);
  sweepwholelist(H, &g->rootgc);
  for (i = 0; i < g->strt.size; i++)  /* free all string lists */
    sweepwholestringlist(H, &g->strt.hash[i]);
}


static void markstrings(hksc_State *H)
{
  global_State *g = G(H);
  int i;
  /* mark all non-fixed strings as dead */
  for (i = 0; i < g->strt.size; i++)
  {
    GCObject *list = g->strt.hash[i];
    GCObject *o;
    for (o = list; o != NULL; o = o->gch.next)
      if (!isfixed(g, o)) makedead(o);
  }
}


void luaC_newcycle (hksc_State *H)
{
  global_State *g = G(H);
  g->gcstate = GCSmarking;
  markstrings(H); /* mark non-fixed strings */
  g->gcstate = GCSsweep;
  sweepwholelist(H, &g->rootgc); /* free all temporary objects */
  g->gcstate = GCSpause;
}


void luaC_step (hksc_State *H) {
  global_State *g = G(H);
  int i;
  g->gcstate = GCSsweepstring; /* removing all dead strings */

  /* for now, just free everything */
  for (i = 0; i < g->strt.size; i++)  /* free all string lists */
    sweepwholestringlist(H, &g->strt.hash[i]);

  checkSizes(H);

  g->gcstate = GCSpause;
#if 0
  l_mem lim = (GCSTEPSIZE/100) * g->gcstepmul;
  if (lim == 0)
    lim = (MAX_LUMEM-1)/2;  /* no limit */
  g->gcdept += g->totalbytes - g->GCthreshold;
  do {
    lim -= singlestep(H);
    if (g->gcstate == GCSpause)
      break;
  } while (lim > 0);
  if (g->gcstate != GCSpause) {
    if (g->gcdept < GCSTEPSIZE)
      g->GCthreshold = g->totalbytes + GCSTEPSIZE;  /* - lim/g->gcstepmul;*/
    else {
      g->gcdept -= GCSTEPSIZE;
      g->GCthreshold = g->totalbytes;
    }
  }
  else {
    lua_assert(g->totalbytes >= g->estimate);
    setthreshold(g);
  }
#endif
}


void luaC_link (hksc_State *H, GCObject *o, lu_byte tt) {
  global_State *g = G(H);
  o->gch.next = g->rootgc;
  g->rootgc = o;
  o->gch.marked = bitmask(TEMPBIT);
  o->gch.tt = tt;
}


void luaC_printstrings (hksc_State *H) {
  UNUSED(H);
}

