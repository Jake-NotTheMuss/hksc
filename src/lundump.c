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
#include "lstring.h"
#include "lundump.h"
#include "lzio.h"

typedef struct {
  hksc_State *H;
  ZIO *Z;
  Mbuffer *b;
  const char *name;
  size_t pos;
  int swapendian;
#ifdef LUA_COD
  int isdebug;
#endif
} LoadState;

#define BADHEADERMSG "Header mismatch when loading bytecode."
#define BADCOMPATMSG BADHEADERMSG " The following build settings differ:"

#define IF(c,s)    if (c) error(S,s)

static void error(LoadState *S, const char *why)
{
  luaD_setferror(S->H,"%s: %s in %s",S->name,why,
#ifdef LUA_COD
                 S->isdebug ? "debug info" :
#endif /* LUA_COD */
                 "precompiled chunk");
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
  LoadVar(S,x);
  correctendianness(S,x);
  IF(x<0, "bad integer");
  return x;
}

#ifdef LUA_COD
static int LoadHash(LoadState *S)
{
  int x;
  LoadVar(S,x);
  correctendianness(S,x);
  return x;
}
#endif /* LUA_COD */

static size_t LoadSize(LoadState *S)
{
  size_t x;
  LoadVar(S,x);
  correctendianness(S,x);
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
  size_t size;
  LoadVar(S,size);
  correctendianness(S,size);
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

static void LoadCode(LoadState *S, Proto *f)
{
  char buf[sizeof(Instruction)];
  int n=cast_int(LoadSize(S));
  LoadBlock(S,buf,(aligned2instr(S->pos) - S->pos));
  f->code=luaM_newvector(S->H,n,Instruction);
  f->sizecode=n;
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
#ifndef LUA_COD
  Proto dummy;
  /* the debug info still needs to be read and advanced over in non-CoD */
  if (hksc_getIgnoreDebug(S->H)) f = &dummy;
#endif /* LUA_COD */
  f->sizelineinfo=LoadInt(S);
  f->sizelocvars=LoadInt(S);
  f->sizeupvalues=LoadInt(S);
  f->linedefined=LoadInt(S);
  f->lastlinedefined=LoadInt(S);
  f->source = LoadString(S);
  if (f->source == NULL) f->source = p;
  f->name = LoadString(S);
  n=f->sizelineinfo;
  f->lineinfo=luaM_newvector(S->H,n,int);
  LoadVector(S,f->lineinfo,n,sizeof(int));
  n=f->sizelocvars;
  f->locvars=luaM_newvector(S->H,n,LocVar);
  for (i=0; i<n; i++) f->locvars[i].varname=NULL;
  for (i=0; i<n; i++)
  {
    f->locvars[i].varname=LoadString(S);
    f->locvars[i].startpc=LoadInt(S);
    f->locvars[i].endpc=LoadInt(S);
  }
  n=f->sizeupvalues;
  f->upvalues=luaM_newvector(S->H,n,TString *);
  for (i=0; i<n; i++) f->upvalues[i]=NULL;
  for (i=0; i<n; i++) f->upvalues[i]=LoadString(S);
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
#ifdef LUA_COD /* Call of Duty also includes a hash after the debug flag */
  if (n==1)
    f->hash=LoadHash(S);
#else /* !LUA_COD */
  if (n!=0)
#endif /* LUA_COD */
  {
    if (debugS != NULL) {
#ifdef LUA_COD
      if (LoadInt(debugS) != 1)
         error(debugS, "bad debug info");
#endif /* LUA_COD */
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
  char typename[MAX_TYPE_LENGTH]; /* buffer for type names */
  int numtypes;
  HkscHeader h, s;
  LoadBlock(S,&s,LUAC_HEADERSIZE);
  S->swapendian = (s.swapendian == 0);
  luaU_header((char *)&h, s.swapendian == 0);
  if (s.compatbits != h.compatbits) /* build settings do not match */
    pushCompatibilityErrorString(S->H, s.compatbits);
  if (memcmp(&h,&s,LUAC_HEADERSIZE)!=0) goto badheader;
  numtypes = LoadInt(S); /* number of types */
  if (numtypes != LUAC_NUMTYPES) {
    if (G(S->H)->bytecode_endianness != HKSC_DEFAULT_ENDIAN)
      goto badheader; /* a specific endianness is expected */
    else { /* try again with swapped endianness */
      swapvarendianness(numtypes);
      if (numtypes != LUAC_NUMTYPES) goto badheader;
      else S->swapendian = !S->swapendian;
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
  Proto *f; /* result */
  LoadState *S; /* reader */
  LoadState *debugS; /* debug reader */
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
#ifdef LUA_COD
  LoadState SD; /* debug load state */
  ZIO ZD; /* debugS->Z */
  Mbuffer buffD; /* debugS->b */
#endif /* LUA_COD */
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
#ifdef LUA_COD
  S.isdebug=0;
#endif /* LUA_COD */
  LoadHeader(&S); /* need some info in the header to initialize debug reader */
#ifdef LUA_COD /* some gymnastics for loading Call of Duty debug files */
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
      SD.isdebug = 1;
    }
    else {
      luaD_throw(S.H,openstatus);
      debugS = NULL; /* to avoid uninitialized variable warning */
    }
  }
  else debugS = NULL; /* do not load debug info */
#else /* !LUA_COD */
  debugS = &S; /* still need to read the embedded debug info if present */
#endif /* LUA_COD */
  u.S = &S;
  u.debugS = debugS;
  status = luaD_pcall(H, f_undump, &u);
#ifdef LUA_COD
  if (G(H)->debugLoadStateClose && !Settings(H).ignore_debug) {
    int closestatus = (*G(H)->debugLoadStateClose)(H, &ZD, &buffD, name);
    if (status == 0 && closestatus != 0)
      status = closestatus;
  }
#endif /* LUA_COD */
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
void luaU_header (char *h, int swapendian)
{
  memcpy(h,LUA_SIGNATURE,sizeof(LUA_SIGNATURE)-1);
  h+=sizeof(LUA_SIGNATURE)-1;
  *h++=(char)LUAC_VERSION;
  *h++=(char)LUAC_FORMAT;
  *h++=(char)(swapendian == 0);        /* endianness */
  *h++=(char)sizeof(int);
  *h++=(char)sizeof(size_t);
  *h++=(char)sizeof(Instruction);
  *h++=(char)sizeof(lua_Number);
  *h++=(char)(((lua_Number)0.5)==0);    /* is lua_Number integral? */
  *h++=(char)compatbits; /* build settings */
  *h++=(char)0; /* true if in a shared state, false for an offline compiler */
}

