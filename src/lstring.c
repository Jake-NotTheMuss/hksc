/*
** $Id: lstring.c,v 2.7 2005/02/18 12:40:02 roberto Exp roberto $
** String table (keeps all strings handled by Lua)
** See Copyright Notice in lua.h
*/


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "hksc_begin_code.h"

#define lstring_c
#define LUA_CORE

#include "lua.h"

#include "lmem.h"
#include "lobject.h"
#include "lstring.h"


void luaS_resize (int newsize) {
  GCObject **newhash;
  stringtable *tb;
  int i;

  newhash = luaM_newvector(newsize, GCObject *);
  tb = &hksc_strt;
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
  luaM_freearray(tb->hash, tb->size, TString *);
  tb->size = newsize;
  tb->hash = newhash;
  printf("resized hksc_strt to %d bytes\n", newsize);
}


static TString *newlstr (const char *str, size_t l,
                                       unsigned int h) {
  TString *ts;
  stringtable *tb;
  if (l+1 > (MAX_SIZET - sizeof(TString))/sizeof(char))
    luaM_toobig();
  ts = cast(TString *, luaM_malloc((l+1)*sizeof(char)+sizeof(TString)));
  ts->tsv.len = l;
  ts->tsv.hash = h;
  /*ts->tsv.marked = luaC_white(G(L));*/
  ts->tsv.tt = LUA_TSTRING;
  ts->tsv.reserved = 0;
  memcpy(ts+1, str, l*sizeof(char));
  ((char *)(ts+1))[l] = '\0';  /* ending 0 */
  tb = &hksc_strt;
  h = lmod(h, tb->size);
  ts->tsv.next = tb->hash[h];  /* chain new entry */
  tb->hash[h] = obj2gco(ts);
  tb->nuse++;
  if (tb->nuse > cast(lu_int32, tb->size) && tb->size <= MAX_INT/2)
    luaS_resize(tb->size*2);  /* too crowded */
  return ts;
}


TString *luaS_newlstr (const char *str, size_t l) {
  char *__str = strndup(str, l);
  printf("encountered string '%s'\n", __str);
  free(__str);

  GCObject *o;
  unsigned int h = cast(unsigned int, l);  /* seed */
  size_t step = (l>>5)+1;  /* if string is too long, don't hash all its chars */
  size_t l1;
  for (l1=l; l1>=step; l1-=step)  /* compute hash */
    h = h ^ ((h<<5)+(h>>2)+cast(unsigned char, str[l1-1]));
  for (o = hksc_strt.hash[lmod(h, hksc_strt.size)];
       o != NULL;
       o = o->gch.next) {
    TString *ts = rawgco2ts(o);
    if (ts->tsv.len == l && (memcmp(str, getstr(ts), l) == 0)) {
      /* string may be dead */
      /*if (isdead(G(L), o)) changewhite(o);*/
      return ts;
    }
  }

  return newlstr(str, l, h);  /* not found */
}

