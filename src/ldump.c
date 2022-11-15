/*
** $Id: ldump.c,v 1.15 2006/02/16 15:53:49 lhf Exp $
** save precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h> /* memset */

#define ldump_c
#define LUA_CORE

#include "lua.h"

#include "lobject.h"
#include "lstate.h"
#include "lundump.h"

/* for TStrings which may be NULL */
#define ts2txt(ts) (((ts) != NULL && getstr(ts) != NULL) ? getstr(ts) : "")

/* macros for testing stripping level properties */
#ifdef LUA_COD
# define needfuncinfo(D) ((D)->striplevel == BYTECODE_STRIPPING_NONE || \
  (D)->striplevel == BYTECODE_STRIPPING_PROFILING || \
  (D)->striplevel == BYTECODE_STRIPPING_ALL)
# define needdebuginfo(D) ((D)->striplevel == BYTECODE_STRIPPING_NONE || \
  (D)->striplevel == BYTECODE_STRIPPING_DEBUG_ONLY)
#else /* !LUA_COD */
# define needfuncinfo(D) 1
# define needdebuginfo(D) ((D)->striplevel == BYTECODE_STRIPPING_NONE)
#endif /* LUA_COD */

typedef struct {
  hksc_State *H;
  lua_Writer writer;
  void *data;
  size_t pos;
  int striplevel;
  int status;
  int swapendian;
} DumpState;

#define DumpMem(b,n,size,D)	DumpBlock(b,(n)*(size),D)
#define DumpVar(x,D)	 	DumpMem(&x,1,sizeof(x),D)

#define correctendianness(D,x) \
  if ((D)->swapendian) swapendianness((char *)&x,sizeof(x))

static void swapendianness(char *x, size_t n) {
  size_t i = 0;
  while (n-- != 0) {
    char t = x[i];
    x[i] = x[n];
    x[n] = t;
    i++;
  }
}

static void DumpBlock(const void *b, size_t size, DumpState *D)
{
  if (D->status==0)
  {
    D->status=(*D->writer)(D->H,b,size,D->data);
    D->pos+=size;
  }
}

static void DumpChar(int y, DumpState *D)
{
  char x=(char)y;
  DumpVar(x,D);
}

static void DumpInt(int x, DumpState* D)
{
  correctendianness(D,x);
  DumpVar(x,D);
}

static void DumpSize(size_t x, DumpState *D)
{
  correctendianness(D,x);
  DumpVar(x, D);
}

static void DumpNumber(lua_Number x, DumpState *D)
{
  correctendianness(D,x);
  DumpVar(x,D);
}
/*
static void DumpVector(const void *b, int n, size_t size, DumpState *D)
{
  DumpInt(n,D);
  DumpMem(b,n,size,D);
}*/

static void DumpString(const TString *s, DumpState *D)
{
  if (s==NULL || getstr(s)==NULL)
  {
    DumpSize(0,D);
  }
  else
  {
    size_t size=s->tsv.len+1;		/* include trailing '\0' */
    DumpSize(size,D);
    DumpBlock(getstr(s),size,D);
  }
}

static void DumpUI64(const lu_int64 x, DumpState *D)
{
  char bytes[8];
#ifdef LUA_UI64_S
#define ui64_byte0 (x.low & 0xff)
#define ui64_byte1 ((x.low >> 8) & 0xff)
#define ui64_byte2 ((x.low >> 16) & 0xff)
#define ui64_byte3 ((x.low >> 24) & 0xff)
#define ui64_byte4 (x.high & 0xff)
#define ui64_byte5 ((x.high >> 8) & 0xff)
#define ui64_byte6 ((x.high >> 16) & 0xff)
#define ui64_byte7 ((x.high >> 24) & 0xff)
#else
#define ui64_byte0 (x & 0xff)
#define ui64_byte1 ((x >> 8) & 0xff)
#define ui64_byte2 ((x >> 16) & 0xff)
#define ui64_byte3 ((x >> 24) & 0xff)
#define ui64_byte4 ((x >> 32) & 0xff)
#define ui64_byte5 ((x >> 40) & 0xff)
#define ui64_byte6 ((x >> 48) & 0xff)
#define ui64_byte7 ((x >> 56) & 0xff)
#endif
  {
    int y=1;
    if ((char)*(char *)&y == 0) { /* big endian */
      bytes[0] = ui64_byte7;
      bytes[1] = ui64_byte6;
      bytes[2] = ui64_byte5;
      bytes[3] = ui64_byte4;
      bytes[4] = ui64_byte3;
      bytes[5] = ui64_byte2;
      bytes[6] = ui64_byte1;
      bytes[7] = ui64_byte0;
    } else { /* little endian */
      bytes[0] = ui64_byte0;
      bytes[1] = ui64_byte1;
      bytes[2] = ui64_byte2;
      bytes[3] = ui64_byte3;
      bytes[4] = ui64_byte4;
      bytes[5] = ui64_byte5;
      bytes[6] = ui64_byte6;
      bytes[7] = ui64_byte7;
    }
  }
  correctendianness(D,bytes);
  DumpVar(bytes,D);
}

static void DumpFunction(const Proto *f, const TString *p, DumpState *D);

static void DumpCode(const Proto *f, DumpState *D)
{
  char buf[HKSC_SIZE_INSTR];
  size_t npadding;
  memset(buf, 0x5f, HKSC_SIZE_INSTR-1);
  buf[HKSC_SIZE_INSTR-1] = '\0';
  DumpSize(f->sizecode,D); /* number of instructions */
  npadding = aligned2instr(D->pos) - D->pos;
  DumpMem(buf,npadding,sizeof(char),D); /* align to next instruction */
  if (!D->swapendian) /* not swapping endianness */
    DumpMem(f->code, f->sizecode, sizeof(Instruction), D);
  else { /* need to swap endianness */
    int i;
    for (i = 0; i < f->sizecode; i++) {
      swapendianness((char *)(f->code+i),HKSC_SIZE_INSTR);
      DumpMem(f->code+i,1,HKSC_SIZE_INSTR,D);
    }
  }
}

static void DumpConstants(const Proto *f, DumpState *D)
{
  int i,n=f->sizek;
  DumpInt(n,D); /* number of constants */
  for (i=0; i<n; i++)
  {
    const TValue *o=&f->k[i];
    DumpChar(ttype(o),D);
    switch (ttype(o))
    {
      case LUA_TNIL:
        DumpChar(LUA_TNIL,D);
        break;
      case LUA_TBOOLEAN:
        DumpChar(LUA_TBOOLEAN,D);
        DumpChar(bvalue(o),D);
        break;
      case LUA_TLIGHTUSERDATA:
        DumpChar(LUA_TLIGHTUSERDATA,D);
        DumpSize(cast(size_t, pvalue(o)),D);
        break;
      case LUA_TNUMBER:
        DumpChar(LUA_TNUMBER,D);
        DumpNumber(nvalue(o),D);
        break;
      case LUA_TSTRING:
        DumpChar(LUA_TSTRING,D);
        DumpString(rawtsvalue(o),D);
        break;
      case LUA_TUI64:
        DumpChar(LUA_TUI64,D);
        DumpUI64(hlvalue(o),D);
        break;
      default:
        lua_assert(0);			/* cannot happen */
        break;
    }
  }
}

static void DumpDebug(const Proto *f, const TString *p, DumpState *D)
{
  int i,n;
  if (D->striplevel == BYTECODE_STRIPPING_ALL) {
    DumpInt(1,D);
    DumpVar(f->hash,D);
  } else if (D->striplevel == BYTECODE_STRIPPING_PROFILING) {
    DumpInt(1,D);
    DumpInt(0,D); /* striplevel line info */
    DumpInt(0,D); /* striplevel local names */
    DumpInt(0,D); /* striplevel upval names */
    DumpInt(f->linedefined,D);
    DumpInt(f->lastlinedefined,D);
    if (p == NULL) /* main chunk */
      DumpString(f->source,D);
    else
      DumpString(NULL,D);
    DumpString(f->name,D);
  } else if (D->striplevel == BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION) {
    n=f->sizelineinfo;
    for (i=0; i<n; i++) {
      const char *line = luaO_pushfstring(D->H, "%u,%u,%s,%u,%s\n", f->hash, i,
        getstr(f->source), f->lineinfo[i], ts2txt(f->name));
      DumpMem(line,strlen(line),sizeof(char),D);
    }
  } else { /* (BYTECODE_STRIPPING_NONE || BYTECODE_STRIPPING_DEBUG_ONLY) */
    DumpInt(1,D);
    DumpInt(f->sizelineinfo,D);
    DumpInt(f->sizelocvars,D);
    DumpInt(f->sizeupvalues,D);
    DumpInt(f->linedefined,D);
    DumpInt(f->lastlinedefined,D);
    if (p == NULL)
      DumpString(f->source,D);
    else
      DumpString(NULL,D);
    DumpString(f->name,D);
    n=f->sizelineinfo;
    for (i=0; i<n; i++)
      DumpInt(f->lineinfo[i],D);
    n=f->sizelocvars;
    for (i=0; i<n; i++) {
      DumpString(f->locvars[i].varname,D);
      DumpInt(f->locvars[i].startpc,D);
      DumpInt(f->locvars[i].endpc,D);
    }
    n=f->sizeupvalues;
    for (i=0; i<n; i++)
      DumpString(f->upvalues[i],D);
  }
}


static void DumpFunction(const Proto *f, const TString *p, DumpState *D)
{
  int i,n;
  if (needfuncinfo(D)) {
    DumpInt(f->nups,D); /* number of upvalues */
    DumpInt(f->numparams,D); /* number of parameters */
    DumpChar(f->is_vararg,D); /* vararg flags */
    DumpInt(f->maxstacksize,D); /* max stack size */
    DumpCode(f,D); /* dump instructions */
    DumpConstants(f,D); /* dump constants */
  }
  DumpDebug(f,p,D);
  n=f->sizep;
  if (needfuncinfo(D))
    DumpInt(n,D); /* number of child functions */
  for (i=0; i<n; i++) DumpFunction(f->p[i],f->source,D);
}

static void DumpHeader(DumpState *D)
{
  if (needfuncinfo(D)) {
    char h[LUAC_HEADERSIZE];
    luaU_header(h, D->swapendian);
    DumpBlock(h,LUAC_HEADERSIZE,D);
    DumpInt(LUAC_NUMTYPES,D); /* number of types */
#define DEFTYPE(t) \
    DumpInt(LUA_##t,D); /* type id */ \
    DumpInt((int)sizeof(#t),D); /* size of type name */ \
    DumpMem(#t,sizeof(#t),sizeof(char),D); /* type name */
#include "ltype.def"
#undef DEFTYPE
  }
}

/*
** dump Lua function as precompiled chunk
*/
int luaU_dump (hksc_State *H, const Proto *f, lua_Writer w, void *data)
{
  int x=1;
  DumpState D;
  D.H=H;
  D.writer=w;
  D.data=data;
  D.pos=0;
  D.striplevel=hksc_getStrip(H);
  D.status=0;
  if ((char)*(char*)&x == 0) /* big endian */
    D.swapendian=(G(H)->endianness==HKSC_LITTLE_ENDIAN);
  else /* little endian */
    D.swapendian=(G(H)->endianness==HKSC_BIG_ENDIAN);
  DumpHeader(&D);
  DumpFunction(f,NULL,&D);
  return D.status;
}
