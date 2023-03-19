/*
** $Id: ltests.c $
** Internal Module for Debugging of the Lua Implementation
** See Copyright Notice in lua.h
*/


#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ltests_c
#define LUA_CORE

#include "hksclua.h"
#include "hksclib.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"



/*
** The whole module only makes sense with LUA_DEBUG on
*/
#if defined(LUA_DEBUG)


int Trick = 0;


static hksc_State *lua_state = NULL;

int islocked = 0;


hksc_State *debug_newstate(hksc_StateSettings *settings)
{
  hksc_State *H;
  hksc_StateSettings default_settings;
  if (!settings) {
    hksI_StateSettings(&default_settings);
    settings = &default_settings;
  }
  settings->frealloc = debug_realloc;
  settings->ud = &memcontrol;
  H = hksI_newstate(settings);
  /*H = lua_newstate(settings);*/
  if (H) {
    luaB_opentests(H);
  }
  return H;
}


/*
** {======================================================================
** Controlled version for realloc.
** =======================================================================
*/

#define MARK    0x55  /* 01010101 (a nice pattern) */

#ifndef EXTERNMEMCHECK
/* full memory check */
#define HEADER  (sizeof(L_Umaxalign)) /* ensures maximum alignment for HEADER */
#define MARKSIZE  16  /* size of marks after each block */
#define blockhead(b)  (cast(char *, b) - HEADER)
#define setsize(newblock, size) (*cast(size_t *, newblock) = size)
#define checkblocksize(b, size) (size == (*cast(size_t *, blockhead(b))))
#define fillmem(mem,size) memset(mem, -MARK, size)
#else
/* external memory check: don't do it twice */
#define HEADER    0
#define MARKSIZE  0
#define blockhead(b)  (b)
#define setsize(newblock, size) /* empty */
#define checkblocksize(b,size)  (1)
#define fillmem(mem,size) /* empty */
#endif


Memcontrol memcontrol = {0L, 0L, 0L, 0L};


static void *checkblock (void *block, size_t size) {
  void *b = blockhead(block);
  int i;
  for (i=0;i<MARKSIZE;i++)
    lua_assert(*(cast(char *, b)+HEADER+size+i) == MARK+i); /* corrupted block? */
  return b;
}


static void freeblock (Memcontrol *mc, void *block, size_t size) {
  if (block) {
    lua_assert(checkblocksize(block, size));
    block = checkblock(block, size);
    fillmem(block, size+HEADER+MARKSIZE);  /* erase block */
    free(block);  /* free original block */
    mc->numblocks--;
    mc->total -= size;
  }
}


void *debug_realloc (void *ud, void *block, size_t oldsize, size_t size) {
  Memcontrol *mc = cast(Memcontrol *, ud);
  lua_assert(oldsize == 0 || checkblocksize(block, oldsize));
  if (mc->memlimit == 0) {  /* first time? */
    char *limit = getenv("MEMLIMIT");  /* initialize memory limit */
    mc->memlimit = limit ? strtoul(limit, NULL, 10) : ULONG_MAX;
  }
  if (size == 0) {
    freeblock(mc, block, oldsize);
    return NULL;
  }
  else if (size > oldsize && mc->total+size-oldsize > mc->memlimit)
    return NULL;  /* to test memory allocation errors */
  else {
    void *newblock;
    int i;
    size_t realsize = HEADER+size+MARKSIZE;
    size_t commonsize = (oldsize < size) ? oldsize : size;
    if (realsize < size) return NULL;  /* overflow! */
    newblock = malloc(realsize);  /* alloc a new block */
    if (newblock == NULL) return NULL;
    if (block) {
      memcpy(cast(char *, newblock)+HEADER, block, commonsize);
      freeblock(mc, block, oldsize);  /* erase (and check) old copy */
    }
    /* initialize new part of the block with something `weird' */
    fillmem(cast(char *, newblock)+HEADER+commonsize, size-commonsize);
    mc->total += size;
    if (mc->total > mc->maxmem)
      mc->maxmem = mc->total;
    mc->numblocks++;
    setsize(newblock, size);
    for (i=0;i<MARKSIZE;i++)
      *(cast(char *, newblock)+HEADER+size+i) = cast(char, MARK+i);
    return cast(char *, newblock)+HEADER;
  }
}


/* }====================================================================== */


const char *const luaT_typenames[] = {
  "nil", "boolean", "userdata", "number",
  "string", "table", "function", "userdata", "thread",
  "ifunction", "cfunction", "ui64", "struct", "",
  "proto", "upval"
};


/*
** {======================================================
** Functions to check memory consistency
** =======================================================
*/

static int testobjref1 (global_State *g, GCObject *f, GCObject *t) {
  UNUSED(f);
  if (isdead(g,t)) return 0;
  else
    return 1;
}


static void printobj (global_State *g, GCObject *o) {
  int i = 0;
  GCObject *p;
  for (p = g->rootgc; p != o && p != NULL; p = p->gch.next) i++;
  if (p == NULL) i = -1;
  printf("%d:%s(%p)-%c(%02X)", i, luaT_typenames[o->gch.tt], (void *)o,
           istemp(g,o)?'t':isdead(g,o)?'b':'w', o->gch.marked);
}


static int testobjref (global_State *g, GCObject *f, GCObject *t) {
  int r = testobjref1(g,f,t);
  if (!r) {
    printf("%d(%02X) - ", g->gcstate, g->currentwhite);
    printobj(g, f);
    printf("\t-> ");
    printobj(g, t);
    printf("\n");
  }
  return r;
}

#define checkobjref(g,f,t) lua_assert(testobjref(g,f,obj2gco(t)))

#define checkvalref(g,f,t) lua_assert(!iscollectable(t) || \
  ((ttype(t) == (t)->value.gc->gch.tt) && testobjref(g,f,gcvalue(t))))



static void checktable (global_State *g, Table *h) {
  int i;
  GCObject *hgc = obj2gco(h);
  if (h->metatable)
    checkobjref(g, hgc, h->metatable);
  i = h->sizearray;
  while (i--)
    checkvalref(g, hgc, &h->array[i]);
  i = sizenode(h);
  while (i--) {
    Node *n = gnode(h, i);
    if (!ttisnil(gval(n))) {
      lua_assert(!ttisnil(gkey(n)));
      checkvalref(g, hgc, gkey(n));
      checkvalref(g, hgc, gval(n));
    }
  }
}


/*
** All marks are conditional because a GC may happen while the
** prototype is still being created
*/
static void checkproto (global_State *g, Proto *f) {
  int i;
  GCObject *fgc = obj2gco(f);
  if (f->source) checkobjref(g, fgc, f->source);
  for (i=0; i<f->sizek; i++) {
    if (ttisstring(f->k+i))
      checkobjref(g, fgc, rawtsvalue(f->k+i));
  }
  for (i=0; i<f->sizeupvalues; i++) {
    if (f->upvalues[i])
      checkobjref(g, fgc, f->upvalues[i]);
  }
  for (i=0; i<f->sizep; i++) {
    if (f->p[i])
      checkobjref(g, fgc, f->p[i]);
  }
  for (i=0; i<f->sizelocvars; i++) {
    if (f->locvars[i].varname)
      checkobjref(g, fgc, f->locvars[i].varname);
  }
}



static void checkobject (global_State *g, GCObject *o) {
  if (isdead(g, o))
/*    lua_assert(g->gcstate == GCSsweepstring || g->gcstate == GCSsweep);*/
{ if (!(g->gcstate == GCSsweepstring || g->gcstate == GCSsweep))
printf(">>> %d  %s  %02x\n", g->gcstate, luaT_typenames[o->gch.tt], o->gch.marked);
}
  else {
    /*if (g->gcstate == GCSfinalize)
      lua_assert(iswhite(o));*/
    switch (o->gch.tt) {
      case LUA_TTABLE: {
        checktable(g, gco2h(o));
        break;
      }
      case LUA_TPROTO: {
        checkproto(g, gco2p(o));
        break;
      }
      default: lua_assert(0);
    }
  }
}


int lua_checkmemory (hksc_State *H) {
  global_State *g = G(H);
  GCObject *o;
  for (o = g->rootgc; o != obj2gco(g->mainthread); o = o->gch.next)
    checkobject(g, o);
  for (o = o->gch.next; o != NULL; o = o->gch.next) {
    lua_assert(o->gch.tt == LUA_TUSERDATA);
    checkobject(g, o);
  }
  return 0;
}

/* }====================================================== */



/*
** {======================================================
** Disassembler
** =======================================================
*/


static char *buildop (Proto *p, int pc, char *buff) {
  Instruction i = p->code[pc];
  OpCode o = GET_OPCODE(i);
  const char *name = luaP_opnames[o];
  int line = getline(p, pc);
  sprintf(buff, "(%4d) %4d - ", line, pc);
  switch (getOpMode(o)) {  
    case iABC:
      sprintf(buff+strlen(buff), "%-12s%4d %4d %4d", name,
              GETARG_A(i), GETARG_B(i), GETARG_C(i));
      break;
    case iABx:
      sprintf(buff+strlen(buff), "%-12s%4d %4d", name, GETARG_A(i), GETARG_Bx(i));
      break;
    case iAsBx:
      sprintf(buff+strlen(buff), "%-12s%4d %4d", name, GETARG_A(i), GETARG_sBx(i));
      break;
  }
  return buff;
}


#if 1
void luaI_printcode (Proto *pt, int size) {
  int pc;
  for (pc=0; pc<size; pc++) {
    char buff[100];
    printf("%s\n", buildop(pt, pc, buff));
  }
  printf("-------\n");
}
#endif


/* }====================================================== */



static void checkfinalmem (void) {
  lua_assert(memcontrol.numblocks == 0);
  lua_assert(memcontrol.total == 0);
}


void luaB_opentests (hksc_State *H) {
  void *ud;
  atexit(checkfinalmem);
  lua_assert(lua_getallocf(H, &ud) == debug_realloc);
  lua_assert(ud == cast(void *, &memcontrol));
  lua_setallocf(H, lua_getallocf(H, NULL), ud);
  lua_state = H;  /* keep first state to be opened */
}

#endif
