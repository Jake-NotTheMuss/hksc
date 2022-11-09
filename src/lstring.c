/*
** $Id: lstring.c,v 2.7 2005/02/18 12:40:02 roberto Exp roberto $
** String table (keeps all strings handled by Lua)
** See Copyright Notice in lua.h
*/


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define lstring_c
#define LUA_CORE

#include "lua.h"

#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"



TString *luaS_mainchunk = NULL;


void luaS_resize (hksc_State *H, int newsize) {
  GCObject **newhash;
  stringtable *tb;
  int i;
  /* TODO: see if this can be an assertion */
  if (G(H)->gcstate == GCSsweepstring)
    return;  /* cannot resize during GC traverse */
  newhash = luaM_newvector(H, newsize, GCObject *);
  tb = &G(H)->strt;
  for (i=0; i<newsize; i++) newhash[i] = NULL;
  /* rehash */
  for (i=0; i<tb->size; i++) {
    GCObject *p = tb->hash[i];
    while (p) {  /* for each node in the list */
      GCObject *next = p->gch.next;  /* save next */
      unsigned int h = gco2ts(p)->hash;
      int h1 = lmod(h, newsize);  /* new position */
      lua_assert(cast_int(h%newsize) == lmod(h, newsize));
      p->gch.next = newhash[h1];  /* chain it */
      newhash[h1] = p;
      p = next;
    }
  }
  luaM_freearray(H, tb->hash, tb->size, TString *);
  tb->size = newsize;
  tb->hash = newhash;
  printf("resized G(H)->strt to %d bytes\n", newsize);
}


static TString *newlstr (hksc_State *H, const char *str, size_t l,
                                       unsigned int h) {
  TString *ts;
  stringtable *tb;
  if (l+1 > (MAX_SIZET - sizeof(TString))/sizeof(char))
    luaM_toobig(H);
  ts = cast(TString *, luaM_malloc(H, (l+1)*sizeof(char)+sizeof(TString)));
  ts->tsv.len = l;
  ts->tsv.hash = h;
  ts->tsv.marked = luaC_white(G(H));
  ts->tsv.tt = LUA_TSTRING;
  ts->tsv.reserved = 0;
  memcpy(ts+1, str, l*sizeof(char));
  ((char *)(ts+1))[l] = '\0';  /* ending 0 */
  tb = &G(H)->strt;
  h = lmod(h, tb->size);
  ts->tsv.next = tb->hash[h];  /* chain new entry */
  tb->hash[h] = obj2gco(ts);
  tb->nuse++;
  if (tb->nuse > cast(lu_int32, tb->size) && tb->size <= MAX_INT/2)
    luaS_resize(H, tb->size*2);  /* too crowded */
  return ts;
}


TString *luaS_newlstr (hksc_State *H, const char *str, size_t l) {
#if 0
  {char *str_x = strndup(str, l);
    if (!str_x) {fprintf(stderr,"strndup returned NULL\n"); exit(EXIT_FAILURE);}
  printf("encountered string \"%s\"\n", str_x);
  free(str_x);}
#endif

  GCObject *o;
  unsigned int h = cast(unsigned int, l);  /* seed */
  size_t step = (l>>5)+1;  /* if string is too long, don't hash all its chars */
  size_t l1;
  for (l1=l; l1>=step; l1-=step)  /* compute hash */
    h = h ^ ((h<<5)+(h>>2)+cast(unsigned char, str[l1-1]));
  for (o = G(H)->strt.hash[lmod(h, G(H)->strt.size)];
       o != NULL;
       o = o->gch.next) {
    TString *ts = rawgco2ts(o);
    if (ts->tsv.len == l && (memcmp(str, getstr(ts), l) == 0)) {
      /* string may be dead */
      if (isdead(G(L), o)) makelive(o);
      return ts;
    }
  }
  return newlstr(H, str, l, h);  /* not found */
}

lu_int32 luaS_dbhashlstr (hksc_State *H, const char *str, size_t l) {
  lu_int32 hash = 5381;
  size_t i = 0;
  UNUSED(H);
  while (i < l) {
    hash = hash * 33 + str[i];
    /* NOTE/TODO: T6 and early versions of T7 incremented i by 2 each loop.
       Current T7 increments by 1, except the PS3 and XBOX360 versions */
    i+=1;
  }
  return hash;
}

