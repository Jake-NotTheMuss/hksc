/*
** $Id: lanalyzer.c $
** Auxiliary functions to manipulate prototype analyzer structures
** See Copyright Notice in lua.h
*/


#include <stddef.h>

#define lanalyzer_c
#define LUA_CORE

#include "hksclua.h"

#include "lanalyzer.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "lzio.h"

#ifdef HKSC_DECOMPILER

Analyzer *luaA_newanalyzer (hksc_State *H) {
  Analyzer *a = luaM_new(H, Analyzer);
  luaC_link(H, obj2gco(a), LUA_TANALYZER);
  a->insproperties = NULL;
  a->sizeinsproperties = 0;
  a->opencalls = NULL;
  a->sizeopencalls = 0;
  a->regnotes = NULL;
  a->sizeregnotes = 0;
  a->regproperties = NULL;
  a->sizeregproperties = 0;
  a->lineinfo = NULL;
  a->sizelineinfo = 0;
  a->locvars = NULL;
  a->sizelocvars = 0;
  a->upvalues = NULL;
  a->sizeupvalues = 0;
  a->bllist.first = a->bllist.last = NULL;
  a->expstack.stk = NULL;
  a->expstack.total = 0;
  a->expstack.used = 0;
  return a;
}


void luaA_freeanalyzer (hksc_State *H, Analyzer *a) {
  struct BlockNode *bn;
  luaM_freearray(H, a->insproperties, a->sizeinsproperties, InstructionFlags);
  luaM_freearray(H, a->opencalls, a->sizeopencalls, struct OpenExpr);
  luaM_freearray(H, a->regnotes, a->sizeregnotes, struct RegNote);
  luaM_freearray(H, a->regproperties, a->sizeregproperties, SlotDesc);
  luaM_freearray(H, a->lineinfo, a->sizelineinfo, int);
  luaM_freearray(H, a->locvars, a->sizelocvars, struct LocVar);
  luaM_freearray(H, a->upvalues, a->sizeupvalues, TString *);
  bn = a->bllist.first;
  while (bn != NULL) {
    struct BlockNode *next = bn->next;
    luaM_free(H, bn);
    bn = next;
  }
  luaM_freearray(H, a->expstack.stk, a->expstack.total, ExpNode);
  luaM_free(H, a);
}


#endif /* HKSC_DECOMPILER */
