/*
** $Id: ldump.c $
** save precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#include <stdio.h> /* TODO:  */
#include <stdlib.h> /* TODO:  */
#include <stddef.h>
#include <string.h> /* memset */

#define ldump_c
#define LUA_CORE

#include "hksclua.h"

#include "ldo.h"
#include "lgc.h"
#include "lopcodes.h"
#include "lobject.h"
#include "lstruct.h"
#include "ltable.h"
#include "lundump.h"


/* macros for testing stripping level properties */
#ifdef LUA_CODT6
# define needfuncinfo(D) ((D)->striplevel == BYTECODE_STRIPPING_NONE || \
  (D)->striplevel == BYTECODE_STRIPPING_PROFILING || \
  (D)->striplevel == BYTECODE_STRIPPING_ALL)
# define needdebuginfo(D) ((D)->striplevel == BYTECODE_STRIPPING_NONE || \
  (D)->striplevel == BYTECODE_STRIPPING_DEBUG_ONLY)
#else /* !LUA_CODT6 */
# define needfuncinfo(D) 1
# define needdebuginfo(D) ((D)->striplevel == BYTECODE_STRIPPING_NONE)
#endif /* LUA_CODT6 */

typedef struct DumpState DumpState;

#ifdef HKSC_MULTIPLAT
typedef void (*DumpTargetInt)(DumpState *D, int x);
typedef void (*DumpTargetSize)(DumpState *D, size_t x);
#endif /* HKSC_MULTIPLAT */

struct DumpState {
  hksc_State *H;
  lua_Writer writer;
  void *data;
  size_t pos;
  int striplevel;
  int status;
  int swapendian;
  struct target_info target;
#ifdef HKSC_MULTIPLAT
  DumpTargetInt dumpint;
  DumpTargetSize dumpsize;
#endif /* HKSC_MULTIPLAT */
#if HKSC_STRUCTURE_EXTENSION_ON
  Table *protos;
  lu_byte seenprotos;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
};

#define DumpMem(b,n,size,D)	DumpBlock(b,(n)*(size),D)
#define DumpVar(x,D)	 	DumpMem(&x,1,sizeof(x),D)
#define DumpCStr(s,D)  DumpMem(s,strlen(s),sizeof(char),D)

static void DumpBlock(const void *b, size_t size, DumpState *D)
{
  if (D->status==0)
  {
    lua_unlock(D->H);
    D->status=(*D->writer)(D->H,b,size,D->data);
    D->pos+=size;
    lua_lock(D->H);
  }
}

static void DumpChar(int y, DumpState *D)
{
  char x=(char)y;
  DumpVar(x,D);
}

static void DumpInt(int x, DumpState* D)
{
#ifndef HKSC_MULTIPLAT
  correctendianness(D,x);
  DumpVar(x,D);
#else /* HKSC_MULTIPLAT */
  (*D->dumpint)(D,x);
#endif /* HKSC_MULTIPLAT */
}

static void DumpSize(size_t x, DumpState *D)
{
#ifndef HKSC_MULTIPLAT
  correctendianness(D,x);
  DumpVar(x, D);
#else /* HKSC_MULTIPLAT */
  (*D->dumpsize)(D,x);
#endif /* HKSC_MULTIPLAT */
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

static void DumpUI64(lu_int64 x, DumpState *D)
{
  /* note that a platform may expect this to be larger than 8 bytes */
  lua_assert(sizeof(lu_int64) == 8);
#ifdef LUA_UI64_S
  correctendianness(D,x.lo);
  correctendianness(D,x.hi);
  if (isbigendian() != D->swapendian) {
    DumpVar(x.hi,D);
    DumpVar(x.lo,D);
  } else { /* little endian */
    DumpVar(x.lo,D);
    DumpVar(x.hi,D);
  }
#else
  (void)isbigendian;
  correctendianness(D,x);
  DumpVar(x,D);
#endif
}

#ifdef HKSC_MULTIPLAT
static void toowide(DumpState *D, const char *what)
{
  hksc_State *H = D->H;
  luaD_setferror(H, "host %s field too wide for target (value too large)",
                 what);
  luaD_throw(H,LUA_ERRRUN);
}

static void DumpInt16(DumpState *D, int x)
{
  unsigned short shrt;
  lua_assert(D->target.sizeint == 2);
  shrt = cast(unsigned short, cast(unsigned int, x));
  if (sizeof(shrt) < sizeof(x) && cast_int(shrt) != x)
    toowide(D,"integer");
  correctendianness(D,shrt);
  DumpVar(shrt,D);
}

static void DumpInt32(DumpState *D, int x)
{
  lu_int32 u32;
  lua_assert(D->target.sizeint == 4);
  u32 = cast(lu_int32, cast(unsigned int, x));
  if (sizeof(u32) < sizeof(x) && cast_int(u32) != x)
    toowide(D,"integer");
  correctendianness(D,u32);
  DumpVar(u32,D);
}

static void DumpSize16(DumpState *D, size_t x)
{
  unsigned short shrt;
  lua_assert(D->target.sizesize == 2);
  shrt = cast(unsigned short, x);
  if (sizeof(shrt) < sizeof(x) && cast(size_t, shrt) != x)
    toowide(D,"size");
  correctendianness(D,shrt);
  DumpVar(shrt,D);
}

static void DumpSize32(DumpState *D, size_t x)
{
  lu_int32 u32;
  lua_assert(D->target.sizesize == 4);
  u32 = cast(lu_int32, x);
  if (sizeof(u32) < sizeof(x) && cast(size_t, u32) != x)
    toowide(D,"size");
  correctendianness(D,u32);
  DumpVar(u32,D);
}

static void DumpSize64(DumpState *D, size_t x)
{
  lu_int64 u64;
  lua_assert(D->target.sizesize == 8);
#ifdef LUA_UI64_S
  if (sizeof(size_t) <= sizeof(lu_int32)) {
    u64.hi = 0;
    u64.lo = cast(lu_int32, x);
  }
  else {
    u64.hi = cast(lu_int32, x >> (CHAR_BIT * 4));
    u64.lo = cast(lu_int32, x & 0xFFFFFFFFlu);
  }
  if (sizeof(size_t) > 8 && ((x >> (CHAR_BIT*8)) != 0))
    toowide(D,"size");
#else
  u64 = cast(lu_int64, x);
  if (sizeof(u64) < sizeof(x) && cast(size_t, u64) != x)
    toowide(D,"size");
#endif /* LUA_UI64_S */
  DumpUI64(u64,D);
}
#endif /* HKSC_MULTIPLAT */

static void DumpFunction(const Proto *f, const TString *p, DumpState *D);

static void DumpCode(const Proto *f, DumpState *D)
{
  char buf[sizeof(Instruction)];
  size_t npadding;
  memset(buf, '_', sizeof(Instruction)-1);
  buf[sizeof(Instruction)-1] = '\0';
  DumpSize(cast(size_t, f->sizecode),D); /* number of instructions */
  npadding = aligned2instr(D->pos) - D->pos;
  DumpMem(buf,npadding,sizeof(char),D); /* align to next instruction */
  if (!D->swapendian) /* not swapping endianness */
    DumpMem(f->code, f->sizecode, sizeof(Instruction), D);
  else { /* need to swap endianness */
    int i;
    for (i = 0; i < f->sizecode; i++) {
      swapendianness(f->code+i,sizeof(Instruction));
      DumpMem(f->code+i,1,sizeof(Instruction),D);
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
        break;
      case LUA_TBOOLEAN:
        DumpChar(bvalue(o),D);
        break;
      case LUA_TLIGHTUSERDATA:
        DumpSize(cast(size_t, pvalue(o)),D);
        break;
      case LUA_TNUMBER:
        DumpNumber(nvalue(o),D);
        break;
      case LUA_TSTRING:
        DumpString(rawtsvalue(o),D);
        break;
      case LUA_TUI64:
        DumpUI64(ui64value(o),D);
        break;
      default:
        lua_assert(0);			/* cannot happen */
        break;
    }
  }
}

#if HKSC_FORMAT_VERSION <= 13
#define aligned(x,n) (((x) + (n-1)) & ~(n-1))

static int sizeofdebuginfo(struct target_info *target)
{
  /*
  struct DebugInfo {
    int line_defined;
    int last_line_defined;
    int sizelineinfo;
    int *lineinfo;
    int sizeupvalues;
    TString **upvalues;
    TString *source;
    TString *name;
    int sizelocvars;
    LocVar *locvars;
  };
  */
  int sizeint = target->sizeint;
  int sizesize = target->sizesize;
  int size = 0;
  size += sizeint * 3; /* line_defined, last_line_defined, sizelineinfo */
  size = aligned(size,sizesize);
  size += sizesize; /* lineinfo */
  size += sizeint; /* sizeupvalues */
  size = aligned(size,sizesize);
  size += sizesize; /* upvalues */
  size += sizesize * 2; /* source, name */
  size += sizeint; /* sizelocvars */
  size = aligned(size,sizesize);
  size += sizesize; /* locvars */
  return size;
}
#endif /* HKSC_FORMAT_VERSION <= 13 */


static void DumpDebug(const Proto *f, const TString *p, DumpState *D)
{
  int i,n;
  if (D->striplevel == BYTECODE_STRIPPING_ALL) {
#ifdef LUA_CODT6
    /* T6/T7 */
    DumpInt(1,D);
    DumpInt(f->hash,D);
#else /* !LUA_CODT6 */
    DumpInt(0,D);
#endif /* LUA_CODT6 */
  } else if (D->striplevel == BYTECODE_STRIPPING_PROFILING) {
#if HKSC_FORMAT_VERSION <= 13
    DumpInt(sizeofdebuginfo(&D->target),D);
#else /* HKSC_FORMAT_VERSION > 13 */
    DumpInt(1,D);
    DumpInt(0,D); /* strip line info */
    DumpInt(0,D); /* strip local names */
    DumpInt(0,D); /* strip upval names */
#endif /* HKSC_FORMAT_VERSION <= 13 */
    DumpInt(f->linedefined,D);
    DumpInt(f->lastlinedefined,D);
    if (p == NULL) /* main chunk */
      DumpString(f->source,D);
    else
      DumpString(NULL,D);
    DumpString(f->name,D);
#if HKSC_FORMAT_VERSION <= 13
    DumpInt(0,D); /* strip line info */
    DumpInt(0,D); /* strip local names */
    DumpInt(0,D); /* strip upval names */
#endif /* HKSC_FORMAT_VERSION <= 13 */
  }
#ifdef LUA_CODT6
  else if (D->striplevel == BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION) {
    char buff[CHAR_BIT * sizeof(lu_int32) * 2];  /* should be enough space */
    n=f->sizelineinfo;
#define PrintString(str,D)  DumpMem(str,strlen(str),sizeof(char),D)
#define PrintTString(ts,D)  DumpMem(getstr(ts),ts->tsv.len,sizeof(char),D)
#define PrintComma(D)  PrintString(",",D)
#define PrintStringf(fmt,var,D) sprintf(buff,fmt,var); PrintString(buff,D)
    for (i=0; i<n; i++) {
      /* <hash>,<i>,<source>,<lineno>,<name> */
      /* print hash */
      PrintStringf("%" LUA_INT_FRMLEN "u", f->hash, D); PrintComma(D);
      /* print line info index */
      PrintStringf("%u", i, D); PrintComma(D);
      /* print source name */
      PrintTString(f->source,D); PrintComma(D);
      /* print line number */
      PrintStringf("%u", f->lineinfo[i], D); PrintComma(D);
      /* print function name */
      if (f->name != NULL)
        /* calculate the length of the name instead of using the TString length
           so the string is only printed up to the first NULL byte (function
           names may contain embedded NULL bytes due to a Havok Script bug, and
           test builds will reproduce the bug, but when dumping callstack
           reconstruction, printf is used, avoiding printing the NULL byte */
        PrintString(getstr(f->name),D);
      PrintString("\n",D);
    }
#undef PrintString
#undef PrintTString
#undef PrintComma
#undef PrintStringf
  }
#endif /* LUA_CODT6 */
  else { /* (BYTECODE_STRIPPING_NONE || BYTECODE_STRIPPING_DEBUG_ONLY) */
#if HKSC_FORMAT_VERSION <= 13
    int sizeneeded = f->sizelocvars*(D->target.sizesize + D->target.sizeint*2) +
                     f->sizeupvalues*(D->target.sizesize) +
                     f->sizelineinfo*(D->target.sizeint);
    DumpInt(sizeneeded+sizeofdebuginfo(&D->target),D);
#else /* HKSC_FORMAT_VERSION > 13 */
    DumpInt(1,D);
    DumpInt(f->sizelineinfo,D);
    DumpInt(f->sizelocvars,D);
    DumpInt(f->sizeupvalues,D);
#endif /* HKSC_FORMAT_VERSION <= 13 */
    DumpInt(f->linedefined,D);
    DumpInt(f->lastlinedefined,D);
    if (p == NULL)
      DumpString(f->source,D);
    else
      DumpString(NULL,D);
    DumpString(f->name,D);
#if HKSC_FORMAT_VERSION <= 13
    DumpInt(f->sizelineinfo,D);
#endif /* HKSC_FORMAT_VERSION <= 13 */
    n=f->sizelineinfo;
    for (i=0; i<n; i++)
      DumpInt(f->lineinfo[i],D);
#if HKSC_FORMAT_VERSION <= 13
    DumpInt(f->sizelocvars,D);
#endif /* HKSC_FORMAT_VERSION <= 13 */
    n=f->sizelocvars;
    for (i=0; i<n; i++) {
      DumpString(f->locvars[i].varname,D);
      DumpInt(f->locvars[i].startpc,D);
      DumpInt(f->locvars[i].endpc,D);
    }
#if HKSC_FORMAT_VERSION <= 13
    DumpInt(f->sizeupvalues,D);
#endif /* HKSC_FORMAT_VERSION <= 13 */
    n=f->sizeupvalues;
    for (i=0; i<n; i++)
      DumpString(f->upvalues[i],D);
  }
}


#if HKSC_STRUCTURE_EXTENSION_ON
static void GetStructuresReferenced(const Proto *f, DumpState *D)
{
  hksc_State *H = D->H;
  Table *t = D->protos;
  TValue key, *val;
  int i;
  lua_assert(t != NULL);
  for (i = 0; i < f->sizecode; i++) {
    Instruction insn = f->code[i];
    switch (GET_OPCODE(insn)) {
      case OP_SETSLOTMT:
        if (GET_SLOTMT_TYPE(insn) != LUA_TSTRUCT)
          break;
        /* fallthrough */
      case OP_NEWSTRUCT:
      case OP_SETSLOTS:
        insn = f->code[++i];  /* the next code has the struct id */
        lua_assert(GET_OPCODE(insn) == OP_DATA);
        /* fallthrough */
      case OP_CHECKTYPES:
      case OP_CHECKTYPE_D: {
        int id = GETARG_Bx(insn);
        if (id != -1) {
          setpvalue(&key, cast(void *, cast(size_t, id)));
          val = luaH_set(H, t, &key);
          if (!ttisboolean(val))
            setbvalue(val, 1);
          D->seenprotos = 1;
        }
        break;
      }
      default: break;
    }
  }
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


static void DumpFunction(const Proto *f, const TString *p, DumpState *D)
{
  int i,n;
#if HKSC_STRUCTURE_EXTENSION_ON
  GetStructuresReferenced(f, D);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
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
    luaU_header(&D->target,h);
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


#if HKSC_STRUCTURE_EXTENSION_ON
static void DumpProto(DumpState *D, Table *t, short id)
{
  size_t i;
  TValue key, *val;
  hksc_State *H = D->H;
  StructProto *proto;
  setpvalue(&key, id2pvalue(id));
  val = luaH_set(H, t, &key);
  if (ttisboolean(val))  /* already dumped this structure */
    return;
  setbvalue(val, 1);
  proto = luaR_getstructbyid(H, id);
  lua_assert(proto != NULL);
  DumpString(proto->name, D);
  DumpSize(cast(size_t, cast(unsigned short, proto->id)), D);
  DumpInt(cast_int(proto->hasmeta), D);
  DumpInt(cast_int(proto->hasproxy), D);
  DumpSize(proto->nslots, D);
  /* dump slots */
  for (i = 0; i < proto->nslots; i++) {
    StructSlot *slot = getprotoslots(proto)+i;
    DumpString(slot->name, D);
    DumpSize(cast(size_t, cast(unsigned short, slot->structid)), D);
    DumpInt(cast_int(slot->typeid), D);
    DumpInt(cast_int(slot->reserved), D);
    DumpSize(cast(size_t, slot->position), D);
  }
  /* dump referenced prototypes */
  for (i = 0; i < proto->nslots; i++) {
    StructSlot *slot = getprotoslots(proto)+i;
    if (slot->typeid == LUA_TSTRUCT)
      DumpProto(D, t, slot->structid);
  }
}


static void DumpStructures(DumpState *D)
{
  hksc_State *H = D->H;
  Table *t = D->protos;
  DumpInt(1,D);
  if (D->seenprotos) {
    Table *dumped_protos = luaH_new(H, 0, sizenode(t));
    TValue s[2];  /* a stack for luaH_next */
    StkId key = &s[0]; StkId val = &s[1];
    setbvalue(key, 1);
    /* insert [true] = dumped_protos */
    val = luaH_set(H, t, key);
    sethvalue(val, dumped_protos);
    setnilvalue(key);
    while (luaH_next(H, t, key)) {
      if (ttislightuserdata(key))
        DumpProto(D, dumped_protos, pvalue2id(key));
    }
    killtemp(obj2gco(dumped_protos));
  }
  DumpSize(0,D);
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


/*
** Execute a protected bytecode dump.
*/
struct SDump {  /* data to `f_dump' */
  DumpState *D;  /* dump state */
  const Proto *f;  /* compiled chunk */
};

static void f_dump (hksc_State *H, void *ud) {
  struct SDump *sd = (struct SDump *)ud;
  DumpState *D = sd->D;
  const Proto *f = sd->f;
#if HKSC_STRUCTURE_EXTENSION_ON
  D->protos = luaH_new(H, 0, 0);
  D->seenprotos = 0;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  DumpHeader(D);
  DumpFunction(f,NULL,D);
#if HKSC_STRUCTURE_EXTENSION_ON
  DumpStructures(D);
  killtemp(obj2gco(D->protos));
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  UNUSED(H);
}

/*
** dump Lua function as precompiled chunk
*/
int luaU_dump (hksc_State *H, const Proto *f, lua_Writer w, void *data)
{
  DumpState D;
  int status;
  struct SDump sd;
  D.H=H;
  D.writer=w;
  D.data=data;
  D.pos=0;
  D.striplevel=hksc_getbytecodestrippinglevel(H);
  D.status=0;
  luaU_target_info(H, &D.target);
  D.swapendian = D.target.needendianswap;
#ifdef HKSC_MULTIPLAT
  if (D.target.sizeint == 2)
    D.dumpint = DumpInt16;
  else if (D.target.sizeint == 4)
    D.dumpint = DumpInt32;
  else
    lua_assert(0);
  if (D.target.sizesize == 2)
    D.dumpsize = DumpSize16;
  else if (D.target.sizesize == 4)
    D.dumpsize = DumpSize32;
  else if (D.target.sizesize == 8)
    D.dumpsize = DumpSize64;
  else
    lua_assert(0);
#endif /* HKSC_MULTIPLAT */
  sd.D=&D;
  sd.f=f;
  status = luaD_pcall(H, f_dump, &sd);
  if (status) D.status = status;
  return D.status;
}
