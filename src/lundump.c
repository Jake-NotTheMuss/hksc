/*
** $Id: lundump.c $
** load precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#include <stddef.h> /* TODO: check inclusiong of stddef and stdarg (included in lua.h) */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define lundump_c
#define LUA_CORE

#include "hksclua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "lundump.h"
#include "lzio.h"

typedef struct LoadState LoadState;

#ifdef HKSC_MULTIPLAT
typedef int (*LoadTargetInt)(LoadState *S);
typedef size_t (*LoadTargetSize)(LoadState *S);
#endif /* HKSC_MULTIPLAT */

struct LoadState {
  hksc_State *H;
  ZIO *Z;
  Mbuffer *b;
  const char *name;
  const char *desc;  /* what is being loaded? */
  size_t pos;
  int swapendian;
  struct target_info target;
#ifdef HKSC_MULTIPLAT
  LoadTargetInt loadint;
  LoadTargetSize loadsize;
#endif /* HKSC_MULTIPLAT */
};

#define BADHEADERMSG "Header mismatch when loading bytecode."
#define BADCOMPATMSG BADHEADERMSG " The following build settings differ:"

#define IF(c,s)    if (c) error(S,s)

static void error(LoadState *S, const char *why)
{
  luaD_setferror(S->H,"%s: %s in %s",S->name,why,S->desc);
  luaD_throw(S->H,LUA_ERRSYNTAX);
}

#define LoadMem(S,b,n,size)  LoadBlock(S,b,(n)*(size))
#define  LoadByte(S)    (lu_byte)LoadChar(S)
#define LoadVar(S,x)    LoadMem(S,&x,1,sizeof(x))
#define LoadVector(S,b,n,size)  LoadMem(S,b,n,size)

static void LoadBlock(LoadState *S, void *b, size_t size)
{
  size_t r=luaZ_read(S->Z,b,size);
  S->pos += size;
  IF(r!=0, "unexpected end");
}

static int LoadChar(LoadState *S)
{
  char x;
  LoadVar(S,x);
  return x;
}

static int LoadInt(LoadState *S)
{
  int x;
#ifndef HKSC_MULTIPLAT
  LoadVar(S,x);
  correctendianness(S,x);
#else /* HKSC_MULTIPLAY */
  x = (*S->loadint)(S);
#endif /* HKSC_MULTIPLAT */
  IF(x<0, "bad integer");
  return x;
}

#ifdef LUA_CODT6
static lu_int32 LoadHash(LoadState *S)
{
  lu_int32 x;
  LoadVar(S,x);
  correctendianness(S,x);
  return x;
}
#endif /* LUA_CODT6 */

static size_t LoadSize(LoadState *S)
{
  size_t x;
#ifndef HKSC_MULTIPLAT
  LoadVar(S,x);
  correctendianness(S,x);
#else /* HKSC_MULTIPLAT */
  x = (*S->loadsize)(S);
#endif /* HKSC_MULTIPLAT */
  return x;
}

static lua_Number LoadNumber(LoadState *S)
{
  lua_Number x;
  LoadVar(S,x);
  correctendianness(S,x);
  return x;
}

static TString *LoadString(LoadState *S)
{
  size_t size = LoadSize(S);
  if (size==0)
    return NULL;
  else
  {
    char *s=luaZ_openspace(S->H,S->b,size);
    LoadBlock(S,s,size);
    return luaS_newlstr(S->H,s,size-1);    /* remove trailing '\0' */
  }
}

static lu_int64 LoadUI64(LoadState *S)
{
  lu_int64 x;
#ifdef LUA_UI64_S
  if (isbigendian()) {
    LoadVar(S,x.hi);
    LoadVar(S,x.lo);
  } else { /* little endian */
    LoadVar(S,x.lo);
    LoadVar(S,x.hi);
  }
  if (S->swapendian) {
    lu_int32 tmp = x.lo;
    x.lo = x.hi;
    x.hi = tmp;
  }
  correctendianness(S,x.hi);
  correctendianness(S,x.lo);
#else
  (void)isbigendian;
  LoadVar(S,x);
  correctendianness(S,x);
#endif
  return x;
}

#ifdef HKSC_MULTIPLAT
static void toowide(LoadState *S, const char *what)
{
  hksc_State *H = S->H;
  luaD_setferror(H, "target %s field too wide for host (value too large)",
                 what);
  luaD_throw(H,LUA_ERRRUN);
}

static int LoadInt16(LoadState *S)
{
  short x;
  lua_assert(S->target.sizeint == 2);
  LoadVar(S,x);
  correctendianness(S,x);
  IF(x<0, "bad integer");
  return cast_int(x);
}

static int LoadInt32(LoadState *S)
{
  l_int32 x;
  int i;
  lua_assert(S->target.sizeint == 4);
  LoadVar(S,x);
  correctendianness(S,x);
  IF(x<0, "bad integer");
  i = cast_int(x);
  if (sizeof(i) < sizeof(x) && cast(l_int32, i) != x)
    toowide(S,"integer");
  return i;
}

static size_t LoadSize16(LoadState *S)
{
  unsigned short x;
  size_t size;
  lua_assert(S->target.sizesize == 2);
  LoadVar(S,x);
  correctendianness(S,x);
  size = cast(size_t, x);
  if (sizeof(size) < sizeof(x) && cast(unsigned short, size) != x)
    toowide(S,"size");
  return size;
}

static size_t LoadSize32(LoadState *S)
{
  lu_int32 x;
  size_t size;
  lua_assert(S->target.sizesize == 4);
  LoadVar(S,x);
  correctendianness(S,x);
  size = cast(size_t, x);
  if (sizeof(size) < sizeof(x) && cast(lu_int32, size) != x)
    toowide(S,"size");
  return size;
}

static size_t LoadSize64(LoadState *S)
{
  lu_int64 x;
  size_t size;
  lua_assert(S->target.sizesize == 8);
  if (sizeof(size_t) == 8) {
    LoadVar(S,size);
    correctendianness(S,size);
  }
  else {
    lu_int32 lo;
    x = LoadUI64(S);
#ifdef LUA_UI64_S
    lo = x.lo;
    if (x.hi != 0)
      toowide(S,"size");
#else /* !LUA_UI64_S */
    lo = cast(lu_int32, (x & 0xFFFFFFFFlu));
    if (cast(lu_int64, lo) != x)
      toowide(S,"size");
#endif /* LUA_UI64_S */
    size = cast(size_t, lo);
    if (sizeof(size) < sizeof(lo) && cast(lu_int32, size) != lo)
      toowide(S,"size");
  }
  return size;
}

#endif /* HKSC_MULTIPLAT */

static void LoadCode(LoadState *S, Proto *f)
{
  char buff[sizeof(Instruction)];
  int n=cast_int(LoadSize(S));
  LoadBlock(S,buff,(aligned2instr(S->pos) - S->pos));
  f->code=luaM_newvector(S->H,n,Instruction);
  f->sizecode=n;
#ifdef HKSC_MULTIPLAT
  if (cast_int(sizeof(Instruction)) < S->target.sizeinstr) {
    luaD_setferror(S->H,"target instruction field is too wide to represent"
                   " (host size is (%d), target size is (%d)",
                   cast_int(sizeof(Instruction)), cast_int(S->target.sizeinstr));
    luaD_throw(S->H,LUA_ERRRUN);
  }
#endif /* HKSC_MULTIPLAT */
  if (!S->swapendian) /* not swapping endianness */
    LoadMem(S, f->code, f->sizecode, sizeof(Instruction));
  else { /* need to swap endianness */
    int i;
    for (i = 0; i < f->sizecode; i++) {
      LoadMem(S,f->code+i,1,sizeof(Instruction));
      swapendianness(f->code+i,sizeof(Instruction));
    }
  }
}

static void enterlevel (LoadState *S) {
  if (++S->H->nCcalls > LUAI_MAXCCALLS)
  error(S, "code too deep");
}


#define leavelevel(S)  ((S)->H->nCcalls--)

static Proto *LoadFunction(LoadState *S, TString *p, LoadState *debugS);

static void LoadConstants(LoadState *S, Proto *f)
{
  int i,n;
  n=LoadInt(S); /* number of constants */
  f->k=luaM_newvector(S->H,n,TValue);
  f->sizek=n;
  for (i=0; i<n; i++) setnilvalue(&f->k[i]);
  for (i=0; i<n; i++)
  {
    TValue *o=&f->k[i];
    int t=LoadChar(S);
    switch (t)
    {
      case LUA_TNIL:
        setnilvalue(o);
        break;
      case LUA_TBOOLEAN:
        setbvalue(o,LoadChar(S));
        break;
      case LUA_TLIGHTUSERDATA:
        setpvalue(o,cast(void *, LoadSize(S)));
        break;
      case LUA_TNUMBER:
        setnvalue(o,LoadNumber(S));
        break;
      case LUA_TSTRING:
        setsvalue2n(o,LoadString(S));
        break;
      case LUA_TUI64:
        setui64value(o,LoadUI64(S));
        break;
      default:
        IF(1, "bad constant");
        break;
    }
  }
}

static void LoadDebug(LoadState *S, Proto *f, TString *p)
{
  int i,n;
#if HKSC_FORMAT_VERSION > 13
  f->sizelineinfo=LoadInt(S);
  f->sizelocvars=LoadInt(S);
  f->sizeupvalues=LoadInt(S);
#endif /* HKSC_FORMAT_VERSION > 13 */
  f->linedefined=LoadInt(S);
  f->lastlinedefined=LoadInt(S);
  f->source = LoadString(S);
  if (f->source == NULL) f->source = p;
  f->name = LoadString(S);
#if HKSC_FORMAT_VERSION <= 13
  f->sizelineinfo=LoadInt(S);
#endif /* HKSC_FORMAT_VERSION <= 13 */
  n=f->sizelineinfo;
  f->lineinfo=luaM_newvector(S->H,n,int);
#ifdef HKSC_MULTIPLAT
  if (S->target.sizeint == sizeof(int)) {
#endif /* HKSC_MULTIPLAT */
  LoadVector(S,f->lineinfo,n,sizeof(int));
#ifdef HKSC_MULTIPLAT
  }
  else {
    for (i=0; i<n; i++) f->lineinfo[i] = LoadInt(S);
  }
#endif /* HKSC_MULTIPLAT */
#if HKSC_FORMAT_VERSION <= 13
  f->sizelocvars=LoadInt(S);
#endif /* HKSC_FORMAT_VERSION <= 13 */
  n=f->sizelocvars;
  f->locvars=luaM_newvector(S->H,n,LocVar);
  for (i=0; i<n; i++) f->locvars[i].varname=NULL;
  for (i=0; i<n; i++)
  {
    f->locvars[i].varname=LoadString(S);
    f->locvars[i].startpc=LoadInt(S);
    f->locvars[i].endpc=LoadInt(S);
  }
#if HKSC_FORMAT_VERSION <= 13
  f->sizeupvalues=LoadInt(S);
#endif /* HKSC_FORMAT_VERSION <= 13 */
  n=f->sizeupvalues;
  f->upvalues=luaM_newvector(S->H,n,TString *);
  for (i=0; i<n; i++) f->upvalues[i]=NULL;
  for (i=0; i<n; i++) f->upvalues[i]=LoadString(S);
#ifndef LUA_CODT6
  /* all the arrays above needed to be allocated in any case to read and
  advance over the data, but now it needs to be freed if ignoring debug info */
  if (hksc_getIgnoreDebug(S->H)) {
    luaM_freearray(S->H, f->lineinfo, f->sizelineinfo, int);
    luaM_freearray(S->H, f->locvars, f->sizelocvars, LocVar);
    luaM_freearray(S->H, f->upvalues, f->sizeupvalues, TString *);
    f->sizelineinfo = 0;
    f->lineinfo = NULL;
    f->sizelocvars = 0;
    f->locvars = NULL;
    f->sizeupvalues = 0;
    f->upvalues = NULL;
  }
#endif /* LUA_CODT6 */
}


static Proto *LoadFunction(LoadState *S, TString *p, LoadState *debugS)
{
  int i,n;
  Proto *f;
  enterlevel(S);
  f=luaF_newproto(S->H); f->source = p;
  f->nups=LoadInt(S); /* number of upvalues */
  f->numparams=LoadInt(S); /* number of parameters */
  f->is_vararg=LoadChar(S); /* vararg flags */
  f->maxstacksize=LoadInt(S); /* max stack size */
  LoadCode(S,f);
  LoadConstants(S,f);
  n=LoadInt(S);
#ifdef LUA_CODT6 /* Call of Duty also includes a hash after the debug flag */
  if (n==1)
    f->hash=LoadHash(S);
#else /* !LUA_CODT6 */
  if (n!=0)
#endif /* LUA_CODT6 */
  {
    if (debugS != NULL) {
#ifdef LUA_CODT6
      if (LoadInt(debugS) != 0)
#endif /* LUA_CODT6 */
      LoadDebug(debugS,f,p);
    }
  }
  n=LoadInt(S); /* number of child functions */
  f->p=luaM_newvector(S->H,n,Proto *);
  f->sizep=n;
  for (i=0; i<n; i++) f->p[i]=NULL;
  for (i=0; i<n; i++) f->p[i]=LoadFunction(S,f->source,debugS);
  IF(!luaG_checkcode(f), "bad code");
  leavelevel(S);
  return f;
}

/* maximum length of a compatibility error message */
#define MAX_COMPAT_ERROR_LENGTH                  \
  (sizeof(BADCOMPATMSG)-1                      + \
   sizeof(" HKSC_GETGLOBAL_MEMOIZATION")-1     + \
   sizeof(" HKSC_STRUCTURE_EXTENSION_ON")-1    + \
   sizeof(" HKSC_SELF")-1                      + \
   sizeof(" HKSC_WITHDOUBLES")-1               + \
   sizeof(" HKSC_WITHNATIVEINT")-1             + \
   1)

static void pushCompatibilityErrorString(hksc_State *H, char bits) {
  char msg[MAX_COMPAT_ERROR_LENGTH];
  strcpy(msg, BADCOMPATMSG);
#define checkcompatbit(bit,flag) \
  if ((bits & (1 << HKSC_COMPATIBILITY_BIT_##bit)) != HKSC_##flag) \
    strcat(msg, " HKSC_" #flag)
  checkcompatbit(MEMOIZATION, GETGLOBAL_MEMOIZATION);
  checkcompatbit(STRUCTURES, STRUCTURE_EXTENSION_ON);
  checkcompatbit(SELF, SELF);
  checkcompatbit(DOUBLES, WITHDOUBLES);
  checkcompatbit(NATIVEINT, WITHNATIVEINT);
  msg[MAX_COMPAT_ERROR_LENGTH-1]='\0';
#undef checkcompatbit
  luaG_runerror(H, msg);
}

static void LoadHeader(LoadState *S)
{
  struct target_info *target = &S->target;
  char typename[MAX_TYPE_LENGTH]; /* buffer for type names */
  int numtypes;
  HkscHeader h, s;
  lua_assert(sizeof(HkscHeader) == LUAC_HEADERSIZE);
  LoadBlock(S,&s,LUAC_HEADERSIZE);
  S->swapendian = (s.endianflag == 0);
#ifdef HKSC_MULTIPLAT
  if (G(S->H)->target_plat == HKSC_TARGET_PLAT_DEFAULT &&
      G(S->H)->target_ws == HKSC_TARGET_WS_DEFAULT) {
    /* one of the features of HKSC_MULTIPLAT - adapt to the word-size of the
       current bytecode stream if no platform is specified */
    target->sizeint = s.sizeint;
    target->sizesize = s.sizesize;
  }
  /* from this point, the target type sizes are in stone, so now the callbacks
     can be set */
  if (target->sizeint == 2)
    S->loadint = LoadInt16;
  else if (target->sizeint == 4)
    S->loadint = LoadInt32;
  else
    lua_assert(0);
  if (target->sizesize == 2)
    S->loadsize = LoadSize16;
  else if (target->sizesize == 4)
    S->loadsize = LoadSize32;
  else if (target->sizesize == 8)
    S->loadsize = LoadSize64;
  else
    lua_assert(0);
#endif /* HKSC_MULTIPLAT */
  /* don't let the swapendian flag cause a header mismatch */
  target->needendianswap = S->swapendian;
  luaU_header(target, (char *)&h);
  if (s.compatbits != h.compatbits) /* build settings do not match */
    pushCompatibilityErrorString(S->H, s.compatbits);
  if (memcmp(&h,&s,LUAC_HEADERSIZE)!=0) goto badheader;
  numtypes = LoadInt(S); /* number of types */
  if (numtypes != LUAC_NUMTYPES) {
#ifndef HKSC_MULTIPLAT
    if (G(S->H)->bytecode_endianness != HKSC_DEFAULT_ENDIAN)
#else /* HKSC_MULTIPLAT */
    if (G(S->H)->target_plat != HKSC_TARGET_PLAT_DEFAULT)
#endif /* HKSC_MULTIPLAT */
      goto badheader; /* a specific endianness is expected */
    else { /* try again with swapped endianness */
      swapvarendianness(numtypes);
      if (numtypes != LUAC_NUMTYPES) goto badheader;
      else { /* good */
        S->swapendian = !S->swapendian;
        target->needendianswap = S->swapendian;
      }
    }
  }
#define DEFTYPE(t) \
  if (LoadInt(S) != LUA_##t) goto badheader; /* type id */ \
  if (LoadInt(S) != (int)sizeof(#t)) goto badheader; /* size of type name */ \
  LoadMem(S,typename,sizeof(#t),sizeof(char)); /* type name */ \
  typename[MAX_TYPE_LENGTH-1]='\0'; \
  if (strcmp(typename,#t) != 0) goto badheader;
#include "ltype.def"
#undef DEFTYPE
  return;
badheader:
  luaG_runerror(S->H, BADHEADERMSG); return;
}


/*
** Execute a protected undump.
*/
struct SUndump {  /* data to `f_undump' */
  Proto *f;  /* result */
  LoadState *S;  /* reader */
  LoadState *debugS;  /* debug reader */
};

static void f_undump (hksc_State *H, void *ud) {
  struct SUndump *u = cast(struct SUndump *, ud);
  u->f = LoadFunction(u->S,luaS_newliteral(H,"=?"),u->debugS);
  if (!u->f->name)
    u->f->name = luaS_newliteral(H, MAINCHUNKNAME);
}

/*
** load precompiled chunk
*/
Proto *luaU_undump (hksc_State *H, ZIO *Z, Mbuffer *buff, const char *name)
{
  struct SUndump u;
  int status;
  LoadState S;
#ifdef LUA_CODT6
  LoadState SD; /* debug load state */
  ZIO ZD; /* debugS->Z */
  Mbuffer buffD; /* debugS->b */
#endif /* LUA_CODT6 */
  LoadState *debugS;
  if (*name=='@' || *name=='=')
    S.name=name+1;
  else if (*name==LUA_SIGNATURE[0])
    S.name="binary string";
  else
    S.name=name;
  S.H=H;
  S.Z=Z;
  S.b=buff;
  S.pos=0;
  S.desc="precompiled chunk";
  /* set initial target values, any corrections are made in `LoadHeader' */
  luaU_target_info(H,&S.target);
  /* need some info in the header before initializing the COD debug reader */
  LoadHeader(&S);
#ifdef LUA_CODT6 /* some gymnastics for loading Call of Duty debug files */
  if (G(H)->debugLoadStateOpen && !Settings(H).ignore_debug) {
    int openstatus = (*G(H)->debugLoadStateOpen)(H, &ZD, &buffD, name);
    if (openstatus == 0) {
      debugS = &SD;
      SD.H = H;
      SD.Z = &ZD;
      SD.b = &buffD;
      SD.swapendian = S.swapendian;
      SD.name = H->currdebugfile;
      SD.pos = 0;
      SD.desc="debug info";
      SD.target = S.target;
#ifdef HKSC_MULTIPLAT
      SD.loadint = S.loadint;
      SD.loadsize = S.loadsize;
#endif /* HKSC_MULTIPLAT */
    }
    else {
      luaD_throw(S.H,openstatus);
      debugS = NULL; /* to avoid uninitialized variable warning */
    }
  }
  else debugS = NULL; /* do not load debug info */
#else /* !LUA_CODT6 */
  debugS = &S; /* still need to read the embedded debug info if present */
#endif /* LUA_CODT6 */
  u.S = &S;
  u.debugS = debugS;
  status = luaD_pcall(H, f_undump, &u);
#ifdef LUA_CODT6
  if (G(H)->debugLoadStateClose && !Settings(H).ignore_debug) {
    int closestatus = (*G(H)->debugLoadStateClose)(H, &ZD, &buffD, name);
    if (status == 0 && closestatus != 0)
      status = closestatus;
  }
#endif /* LUA_CODT6 */
  if (status)
    luaD_throw(H, status); /* return the error to the outer pcall */
  return u.f;
}


#define compatbits \
  ((HKSC_GETGLOBAL_MEMOIZATION   << HKSC_COMPATIBILITY_BIT_MEMOIZATION) | \
  (HKSC_STRUCTURE_EXTENSION_ON   << HKSC_COMPATIBILITY_BIT_STRUCTURES)  | \
  (HKSC_SELF                     << HKSC_COMPATIBILITY_BIT_SELF)        | \
  (HKSC_WITHDOUBLES              << HKSC_COMPATIBILITY_BIT_DOUBLES)     | \
  (HKSC_WITHNATIVEINT            << HKSC_COMPATIBILITY_BIT_NATIVEINT))

/*
* make header
*/
void luaU_header (struct target_info *target, char *h)
{
  memcpy(h,LUA_SIGNATURE,sizeof(LUA_SIGNATURE)-1);
  h+=sizeof(LUA_SIGNATURE)-1;
  *h++=(char)LUAC_VERSION;
  *h++=(char)LUAC_FORMAT;
  *h++=(char)(target->needendianswap == 0);        /* endianness */
  *h++=(char)(target->sizeint);
  *h++=(char)(target->sizesize);
  *h++=(char)(target->sizeinstr);
  *h++=(char)sizeof(lua_Number);
  *h++=(char)(((lua_Number)0.5)==0);    /* is lua_Number integral? */
  *h++=(char)compatbits; /* build settings */
  *h++=(char)0; /* true if in a shared state, false for an offline compiler */
}


/* set target type sizes and endianness */
void luaU_target_info (hksc_State *H, struct target_info *target)
#ifdef HKSC_MULTIPLAT
{
  int sizeint, sizesize, sizeinstr, bigendian;
  switch (G(H)->target_plat) {
    case HKSC_TARGET_PLAT_DEFAULT:
      bigendian = isbigendian();
      break;
    case HKSC_TARGET_PLAT_WII:
    case HKSC_TARGET_PLAT_CAFE:
    case HKSC_TARGET_PLAT_PS3:
    case HKSC_TARGET_PLAT_XENON:
      sizeint = 4;
      sizesize = 4;
      sizeinstr = 4;
      bigendian = 1;
      break;
    case HKSC_TARGET_PLAT_PSV:
      sizeint = 4;
      sizesize = 4;
      sizeinstr = 4;
      bigendian = 0;
    case HKSC_TARGET_PLAT_ORBIS:
    case HKSC_TARGET_PLAT_DURANGO:
    case HKSC_TARGET_PLAT_WINDOWS:
    case HKSC_TARGET_PLAT_GNU:
    case HKSC_TARGET_PLAT_DARWIN:
      sizeint = 4;
      sizesize = 8;
      sizeinstr = 4;
      bigendian = 0;
      break;
    default:
      lua_assert(0);
  }
  switch (G(H)->target_ws) {
    case HKSC_TARGET_WS_16:
      sizeint = 2;
      sizesize = 2;
      sizeinstr = 4;
      break;
    case HKSC_TARGET_WS_32:
      sizeint = 4;
      sizesize = 4;
      sizeinstr = 4;
      break;
    case HKSC_TARGET_WS_64:
      sizeint = 4;
      sizesize = 8;
      sizeinstr = 4;
      break;
    default:
      if (G(H)->target_plat == HKSC_TARGET_PLAT_DEFAULT) {
        sizeint = sizeof(int);
        sizesize = sizeof(size_t);
        sizeinstr = sizeof(Instruction);
      }
      break;
  }
  target->sizeint = sizeint;
  target->sizesize = sizesize;
  target->sizeinstr = sizeinstr;
  target->needendianswap = (bigendian != isbigendian());
}
#else /* !HKSC_MULTIPLAT */
{
  target->sizeint = sizeof(int);
  target->sizesize = sizeof(size_t);
  if (isbigendian())
    target->needendianswap=(G(H)->bytecode_endianness==HKSC_LITTLE_ENDIAN);
  else /* little endian */
    target->needendianswap=(G(H)->bytecode_endianness==HKSC_BIG_ENDIAN);
}
#endif /* HKSC_MULTIPLAT */

