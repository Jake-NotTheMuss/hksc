/*
** $Id: lzio.c,v 1.30 2005/05/17 19:49:15 roberto Exp roberto $
** a generic input stream interface
** See Copyright Notice in lua.h
*/


#include <string.h>

#define lzio_c
#define LUA_CORE

#include "lua.h"

#include "llimits.h"
#include "lmem.h"
#include "lstate.h"
#include "lzio.h"


int luaZ_fill (ZIO *z) {
  size_t size;
  hksc_State *H = z->H;
  const char *buff;
  lua_unlock(H);
  buff = z->reader(H, z->data, &size);
  lua_lock(H);
  if (buff == NULL || size == 0) { /* end of stream */
    z->state = STREAM_END;
    z->n = 0;
    return 0;
  } else {
    z->n = size - 1;
    z->p = buff;
    return char2int(*(z->p++));
  }
}


int luaZ_lookahead (ZIO *z) {
  if (z->n == 0) {
    luaZ_fill(z);
    if (z->state != STREAM_OK)
      return 0;
    else {
      z->n++;  /* luaZ_fill removed first byte; put back it */
      z->p--;
    }
  }
  return char2int(*z->p);
}


void luaZ_init (hksc_State *H, ZIO *z, lua_Reader reader, void *data) {
  z->H = H;
  z->reader = reader;
  z->data = data;
  z->n = 0;
  z->p = NULL;
  z->state = STREAM_OK;
}


/* --------------------------------------------------------------- read --- */
size_t luaZ_read (ZIO *z, void *b, size_t n) {
  while (n) {
    size_t m;
    if (z->n == 0) luaZ_fill(z);
    if (z->state != STREAM_OK)
      return n;  /* return number of missing bytes */
    m = (n <= z->n) ? n : z->n;  /* min. between n and z->n */
    memcpy(b, z->p, m);
    z->n -= m;
    z->p += m;
    b = (char *)b + m;
    n -= m;
  }
  return 0;
}

/* ------------------------------------------------------------------------ */
char *luaZ_openspace (hksc_State *H, Mbuffer *buff, size_t n) {
  if (n > buff->buffsize) {
    if (n < LUA_MINBUFFER) n = LUA_MINBUFFER;
    luaZ_resizebuffer(H, buff, n);
  }
  return buff->buffer;
}


