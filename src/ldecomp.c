/*
** $Id: ldecomp.c $
** decompile precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h> /* memset */

#define ldecomp_c
#define LUA_CORE

#include "lua.h"

#include "lobject.h"
#include "lstate.h"
#include "lundump.h"

typedef struct {
  hksc_State *H;
  lua_Writer writer;
  void *data;
  int status;
} DecompState;

/*
** dump Lua function as decompiled chunk
*/
int luaU_decompile (hksc_State *H, const Proto *f, lua_Writer w, void *data)
{
  DecompState D;
  D.H=H;
  D.writer=w;
  D.data=data;
  D.status=0;
/*  DumpHeader(&D);
  DumpFunction(f,NULL,&D);*/
  return D.status;
}
