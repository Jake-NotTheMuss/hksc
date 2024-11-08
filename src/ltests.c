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


LUA_API hksc_State *debug_newstate(hksc_StateSettings *settings)
{
  hksc_State *H;
  hksc_StateSettings default_settings;
  if (!settings) {
    hksI_settings(&default_settings);
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
#define HEADER  (sizeof(L_Umaxalign))/* ensures maximum alignment for HEADER */
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
    /* corrupted block? */
    lua_assert(*(cast(char *, b)+HEADER+size+i) == MARK+i);
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
      sprintf(buff+strlen(buff), "%-12s%4d %4d", name, GETARG_A(i),
              GETARG_Bx(i));
      break;
    case iAsBx:
      sprintf(buff+strlen(buff), "%-12s%4d %4d", name, GETARG_A(i),
              GETARG_sBx(i));
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
