/*
** $Id: lgc.c $
** Garbage Collector
** See Copyright Notice in lua.h
*/

#include <string.h>

#define lgc_c
#define LUA_CORE

#include "hksclua.h"

#include "lanalyzer.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"


#define setthreshold(g)  (g->GCthreshold = (2*g->totalbytes))



static void freeobj (hksc_State *H, GCObject *o) {
  switch (o->gch.tt) {
#ifdef HKSC_DECOMPILER
    case LUA_TANALYZER: luaA_freeanalyzer(H, gco2a(o)); break;
#endif /* HKSC_DECOMPILER */
    case LUA_TTYPEANALYZER: luaY_freetypeanalyzer(H, gco2ta(o)); break;
    case LUA_TPROTO: luaF_freeproto(H, gco2p(o)); break;
    case LUA_TTABLE: luaH_free(H, gco2h(o)); break;
    case LUA_TTHREAD: {
      lua_assert(gco2th(o) != H && gco2th(o) != G(H)->mainthread);
      break;
    }
    case LUA_TSTRING: {
      G(H)->strt.nuse--;
      luaM_freemem(H, o, sizestring(gco2ts(o)));
      break;
    }
    case LUA_TUSERDATA: {
      luaM_freemem(H, o, sizeudata(gco2u(o)));
      break;
    }
    default: lua_assert(0);
  }
}



#define sweepwholelist(H,p) sweeplist(H,p)


static GCObject **sweeplist (hksc_State *H, GCObject **p) {
  GCObject *curr;
  global_State *g = G(H);
  int deadmask = otherwhite(g);
  while ((curr = *p) != NULL) {
    if (curr->gch.marked & deadmask) {  /* not dead? */
      lua_assert(!isdead(g, curr) || testbit(curr->gch.marked, FIXEDBIT) ||
                 (g->midcycle && istemp(g, curr)));
      makelive(curr);  /* make it white (for next cycle) */
      p = &curr->gch.next;
    }
    else {  /* must erase `curr' */
      lua_assert(isdead(g, curr) || deadmask == bitmask(SFIXEDBIT));
      *p = curr->gch.next;
      if (curr == g->rootgc)  /* is the first element of the list? */
        g->rootgc = curr->gch.next;  /* adjust first */
      freeobj(H, curr);
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
    sweepwholelist(H, &g->strt.hash[i]);
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
  lua_assert(g->midcycle == 0);
  g->currentwhite = bitmask(FIXEDBIT);
  markstrings(H); /* mark non-fixed strings */
  sweepwholelist(H, &g->rootgc); /* collect tables and protypes */
  luaC_checkGC(H); /* maybe collect strings */
}


void luaC_collectgarbage (hksc_State *H) {
  global_State *g = G(H);
  lu_mem old = g->totalbytes;
  int i;
  if (g->midcycle)
    /* do not collect temp objects mid-cycle */
    g->currentwhite |= bitmask(TEMPBIT);
  else
    /* in-between cycles, collect temp objects */
    g->currentwhite &= ~bitmask(TEMPBIT);
  sweepwholelist(H, &g->rootgc);
  for (i=0; i<g->strt.size; i++) {  /* for each list */
    sweeplist(H, &g->strt.hash[i]);
  }
  lua_assert(old >= g->totalbytes);
  UNUSED(old); /* avoid warning */
  checkSizes(H);
  setthreshold(g);
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

