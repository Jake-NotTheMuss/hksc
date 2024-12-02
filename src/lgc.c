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


typedef struct GCState {
  GCObject *tmark;  /* list of marked objects to be traversed */
  global_State *g;
} GCState;


#define setbit(x,b) l_setbit(x,b)

#define unmark(x) resetbit((x)->gch.marked, GC_REACHABLE);
#define ismarked(x) ((x)->gch.marked & bitmask(GC_REACHABLE))

#define stringmark(s) setbit((s)->tsv.marked, GC_REACHABLE)


#define markvalue(st,o) (checkconsistency(o), \
(iscollectable(o) && !ismarked(gcvalue(o))) ? \
reallymarkobject(st,gcvalue(o)) : (void)0)

#define markobject(st,t) \
  (!ismarked(obj2gco(t)) ? reallymarkobject(st, obj2gco(t)) : (void)0)



static void reallymarkobject (GCState *st, GCObject *o) {
  lua_assert(!ismarked(o));
  setbit(o->gch.marked, GC_REACHABLE);  /* mark object */
  switch (o->gch.tt) {
    case LUA_TUSERDATA:
      if (gco2u(o)->metatable)
        markobject(st, gco2u(o)->metatable);
      break;
    case LUA_TFUNCTION:
    case LUA_TIFUNCTION:
    case LUA_TCFUNCTION:
      gco2cl(o)->c.gclist = st->tmark;
      st->tmark = o;
      break;
    case LUA_TTABLE:
      gco2h(o)->gclist = st->tmark;
      st->tmark = o;
      break;
    case LUA_TTHREAD:
      gco2th(o)->gclist = st->tmark;
      st->tmark = o;
      break;
    case LUA_TPROTO:
      gco2p(o)->gclist = st->tmark;
      st->tmark = o;
      break;
#if HKSC_STRUCTURE_EXTENSION_ON
    case LUA_TSTRUCT:
      /* there are no struct instances in the compiler */
      break;
#endif
    default: lua_assert(o->gch.tt == LUA_TSTRING);
  }
}


static void traversetable (GCState *st, Table *h) {
  int i;
  if (h->metatable) markobject(st, h->metatable);
  i = h->sizearray;
  while (i--)
    markvalue(st, &h->array[i]);
  i = sizenode(h);
  while (i--) {
    Node *n = gnode(h, i);
    TValue *k = key2tval(n), *v = gval(n);
    if (!ttisnil(k)) {
      lua_assert(!ttisnil(v));
      markvalue(st, k);
      markvalue(st, v);
    }
  }
}


/*
** All marks are conditional because a GC may happen while the
** prototype is still being created
*/
static void traverseproto (GCState *st, Proto *f) {
  int i;
  if (f->source) stringmark(f->source);
  for (i=0; i<f->sizek; i++)  /* mark literals */
    markvalue(st, &f->k[i]);
  for (i=0; i<f->sizeupvalues; i++) {  /* mark upvalue names */
    if (f->upvalues[i])
      stringmark(f->upvalues[i]);
  }
  for (i=0; i<f->sizep; i++) {  /* mark nested protos */
    if (f->p[i])
      markobject(st, f->p[i]);
  }
  for (i=0; i<f->sizelocvars; i++) {  /* mark local-variable names */
    if (f->locvars[i].varname)
      stringmark(f->locvars[i].varname);
  }
}


#if HKSC_STRUCTURE_EXTENSION_ON
static void markstructprotos (GCState *st) {
  global_State *g = st->g;
  int i;
  for (i = 0; i < g->protolist.nuse; i++) {
    StructProto *p = g->protolist.list[i];
    int j, n = p->nslots;
    stringmark(p->name);
    for (j = 0; j < n; j++) {
      StructSlot *slot = &p->slots[j];
      stringmark(slot->name);
    }
  }
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


static void propagatemarks (GCState *st) {
  while (st->tmark) {  /* traverse marked objects */
    switch (st->tmark->gch.tt) {
      case LUA_TTABLE: {
        Table *h = gco2h(st->tmark);
        st->tmark = h->gclist;
        traversetable(st, h);
        break;
      }
      case LUA_TPROTO: {
        Proto *p = gco2p(st->tmark);
        st->tmark = p->gclist;
        traverseproto(st, p);
        break;
      }
      default:;
    }
  }
}


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


static int sweeplist (hksc_State *H, GCObject **p, int limit) {
  GCObject *curr;
  int count = 0;  /* number of collected items */
  while ((curr = *p) != NULL) {
    if (curr->gch.marked > limit) {
      unmark(curr);
      p = &curr->gch.next;
    }
    else {
      count++;
      *p = curr->gch.next;
      freeobj(H, curr);
    }
  }
  return count;
}


static void sweepstrings (hksc_State *H, int limit) {
  int i;
  for (i=0; i<G(H)->strt.size; i++) {  /* for each list */
    G(H)->strt.nuse -= sweeplist(H, &G(H)->strt.hash[i], limit);
  }
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


/* called before every translation cycle */
void luaC_newcycle (hksc_State *H) {
  global_State *g = G(H);
  int i;
  lua_assert(g->midcycle == 0);
  /* mark dead strings */
  for (i = 0; i < g->strt.size; i++) {
    GCObject *o, *list = g->strt.hash[i];
    for (o = list; o != NULL; o = o->gch.next)
      unmark(o);
  }
  /* collect internal representations */
  sweeplist(H, &g->rootgc, bitmask(GC_FIXED)-1);
  /* maybe collect strings */
  luaC_checkGC(H);
}


void luaC_sweep (hksc_State *H, int kind) {
  int limit;
  if (kind == GC_COLLECT_ALL) limit = 256;  /* larger than any mark */
  else if (kind == GC_COLLECT_NORMAL) limit = bitmask(GC_TEMP);
  else limit = 0;
  sweeplist(H, &G(H)->rootgc, limit);
  sweepstrings(H, limit);
}


/* mark root set */
static void markroot (GCState *st, hksc_State *H) {
  int i;
#if HKSC_STRUCTURE_EXTENSION_ON
  if (st->g->prototable)
    markobject(st, st->g->prototable);
  markstructprotos(st);
#endif
  if (H->last_result)
    markobject(st, H->last_result);
  for (i = 0; i < st->g->prefixmaps.used; i++) {
    FilePrefixMap *map = st->g->prefixmaps.s + i;
    stringmark(map->from);
    stringmark(map->to);
  }
}


static void mark (hksc_State *H) {
  GCState st;
  st.g = G(H);
  st.tmark = NULL;
  markroot(&st, H);
  propagatemarks(&st);  /* mark all reachable objects */
}


void luaC_collectgarbage (hksc_State *H) {
  global_State *g = G(H);
  mark(H);
  luaC_sweep(H, g->midcycle ? GC_COLLECT_KEEP_TEMP : GC_COLLECT_NORMAL);
  checkSizes(H);
  setthreshold(g);
}


void luaC_link (hksc_State *H, GCObject *o, lu_byte tt) {
  o->gch.next = G(H)->rootgc;
  G(H)->rootgc = o;
  o->gch.marked = bitmask(GC_TEMP);
  o->gch.tt = tt;
}


void luaC_printstrings (hksc_State *H) {
  UNUSED(H);
}

