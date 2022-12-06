/*
** $Id: hkscdump.c $
** Dumping bytecode to files
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <errno.h>

#define hkscdump_c
#define LUA_CORE

#include "lua.h"
#include "luaconf.h"

#include "hksclib.h"

#include "lctype.h"
#include "ldo.h"
#include "lobject.h"
#include "lstring.h"
#include "lundump.h"

extern const char *output;

#ifdef LUA_COD
extern const char *debugfile;
extern const char *callstackdb;
/*extern const char *debugext;*/
#endif /* LUA_COD */


/* default extension of pre-compiled Lua files */
#define LUAC_EXT ".luac"
/* default extension of source Lua files */
#define LUA_EXT  ".lua"
/* default extension of decompiled Lua files */
#define LUADEC_EXT  ".dec.lua"

#ifdef LUA_COD
/* default extension of callstack reconstruction files */
# define LUACALLSTACK_EXT ".luacallstackdb"
/* defalt extension of debug info files */
# define LUADEBUG_EXT ".luadebug"
#endif /* LUA_COD */


#define hasluaext(s,l) (hasext(s,l,LUA_EXT))
#define hasluacext(s,l) (hasext(s,l,LUAC_EXT))


#define MAXEXT 32 /* arbitrary limit on the length of file extensions */

/* returns whether a file name has a given extension (C89, case-insensative) */
static int hasext(const char *name, size_t namelen, const char *ext) {
  char buff[MAXEXT];
  size_t n;
  size_t extlen;
  lua_assert(ext != NULL);
  lua_assert(*ext == '.');
  extlen = strlen(ext);
  lua_assert(extlen < MAXEXT);
  if (namelen < extlen || extlen < 2) return 0;
  for (n = 0; n < extlen; n++)
    buff[n]=ltolower(name[namelen-(extlen-n)]);
  buff[n]='\0';
  return (strcmp(buff, ext) == 0);
}

#define lua2luac(H,name) lua2ext(H,name,LUAC_EXT)
#define luac2luadec(H,name) lua2ext(H,name,LUADEC_EXT)
#ifdef LUA_COD
#define lua2luacallstackdb(H,name) lua2ext(H,name,LUACALLSTACK_EXT)
#define lua2luadebug(H,name) lua2ext(H,name,LUADEBUG_EXT)
#endif /* LUA_COD */

#define MAXLUACNAME PATH_MAX

/*
** generate a name for a an output file from a given input name
*/
static const char *lua2ext(hksc_State *H, const char *name, const char *ext) {
  char buff[MAXLUACNAME];
  size_t n = 0;
  size_t namelen = strlen(name);
  size_t extlen = strlen(ext);
  if (hasluacext(name,namelen)) /* `*.luac'? */
    namelen-=sizeof(LUAC_EXT)-1; /* remove `.luac' */
  else if (hasluaext(name,namelen)) /* `*.lua'? */
    namelen-=sizeof(LUA_EXT)-1; /* remove `.lua' */
  if ((namelen+extlen+1) >= MAXLUACNAME)
    namelen = MAXLUACNAME-(extlen+1);
  memcpy(buff, name, namelen);
  n+=namelen;
  memcpy(buff+n, ext, extlen);
  n+=extlen;
  buff[n]='\0';
  return getstr(luaS_newlstr(H, buff, n));
}

static int writer_2file(hksc_State *H, const void *p, size_t size, void *u) {
  UNUSED(H);
  return (fwrite(p,size,1,(FILE*)u)!=1) && (size!=0);
}

/* push error string "cannot <what> <filename>" */
#define cannot(what,name) do { \
  hksc_setfmsg(H, "cannot " what " %s: %s", name, strerror(errno)); \
  return LUA_ERRFILE; \
} while (0)

#define xopenfile(var,name,mode) do { \
  var = fopen(name,mode); \
  if (var==NULL) cannot("open",name); \
} while (0)

#ifdef LUA_COD
# ifdef HKSC_DECOMPILER /* (COD) debug loader stuff */

typedef struct LoadDebug
{
  FILE *f;
  char buff[LUAL_BUFFERSIZE];
} LoadDebug;

static const char *debug_reader (hksc_State *H, void *ud, size_t *size) {
  LoadDebug *ld = (LoadDebug *)ud;
  UNUSED(H);
  if (feof(ld->f)) return NULL;
  *size = fread(ld->buff, 1, LUAL_BUFFERSIZE, ld->f);
  return (*size > 0) ? ld->buff : NULL;
}

int init_debug_reader(hksc_State *H, ZIO *z, Mbuffer *buff, const char *name) {
  LoadDebug *ld = luaM_new(H, LoadDebug);
  lua_assert(Settings(H).ignore_debug == 0);
  if (H->currdebugfile == NULL) {
    hksc_setfmsg(H, "debug file name not set for input `%s'", name);
    return LUA_ERRRUN;
  }
  ld->f = fopen(H->currdebugfile, "rb");
  if (ld->f == NULL) cannot("open", H->currdebugfile);
  luaZ_init(H, z, debug_reader, ld);
  luaZ_initbuffer(H, buff);
  return 0;
}

int close_debug_reader(hksc_State *H, ZIO *z, Mbuffer *buff, const char *name) {
  int readstatus, closestatus;
  LoadDebug *ld = z->data;
  lua_assert(Settings(H).ignore_debug == 0);
  (void)z; (void)name;
  if (ld->f == NULL) return 0;
  luaZ_freebuffer(H, buff);
  luaM_free(H, ld);
  readstatus = ferror(ld->f);
  closestatus = fclose(ld->f);
  if (readstatus) cannot("read", H->currdebugfile);
  if (closestatus) cannot("close", H->currdebugfile);
  return 0;
}

# endif /* HKSC_DECOMPIELR */

void luacod_startcycle(hksc_State *H, const char *name) {
  if (!Settings(H).ignore_debug) {
    if (debugfile == NULL) /* may be provided in command line */
      debugfile = lua2luadebug(H, name);
    if (callstackdb == NULL)
      callstackdb = lua2luacallstackdb(H, name);
# ifdef HKSC_DECOMPILER
    H->currdebugfile = debugfile;
# endif /* HKSC_DECOMPILER */
  }
}

void luacod_endcycle(hksc_State *H, const char *name) {
  (void)H; (void)name;
  /* output names are only generated if they were previously NULL; this works
     even if output names were explicitly provided, since only 1 cycle should
     run in that case */
  debugfile = NULL;
  callstackdb = NULL;
# ifdef HKSC_DECOMPILER
    H->currdebugfile = NULL;
# endif /* HKSC_DECOMPILER */
}

#define dumpdebugfile(name, striplevel, mode) do { \
  FILE *debug_file_; \
  xopenfile(debug_file_,name,mode); \
  lua_lock(H); \
  hksc_setBytecodeStrippingLevel(H,striplevel); \
  luaU_dump(H,f,writer_2file,debug_file_); \
  lua_unlock(H); \
  if (ferror(debug_file_)) cannot("write",name); \
  if (fclose(debug_file_)) cannot("close",name); \
} while (0)


/* (COD) dump debug info to files */
static int luacod_dumpdebug(hksc_State *H, const Proto *f) {
  if (Settings(H).ignore_debug) return 0;
  /* debug information */
  dumpdebugfile(debugfile,BYTECODE_STRIPPING_DEBUG_ONLY,"wb");
  /* callstack reconstruction */
  dumpdebugfile(callstackdb,BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION,"w");
  hksc_setBytecodeStrippingLevel(H,BYTECODE_STRIPPING_ALL); /* reset */
  return 0;
}
#endif /* LUA_COD */

/* default dump function used by standalone program (for bytecode or decomp) */
int hksc_dump_function(hksc_State *H, const Proto *f, const char *filename) {
  int status;
  FILE *out; /* output file */
  const char *outname; /* output file name */
  const int compiling = luaE_mode(H) == HKSC_MODE_COMPILE;
  lua_assert(f != NULL); /* parse errors should be caught before calling */
#ifdef LUA_COD
  if (compiling) { /* (COD) dump debug info to separate files */
    status = luacod_dumpdebug(H, f);
    if (status) return status; /* error */
  }
#endif /* LUA_COD */
  if (output == NULL || *output == 0) { /* generate an output name if needed */
    if (compiling)
      outname = lua2luac(H, filename);
    else
      outname = luac2luadec(H, filename);
  } else
    outname = output;
  lua_assert(outname != NULL);
  out = fopen(outname, compiling ? "wb" : "w");
  if (out == NULL) cannot("open", outname);
  if (compiling) /* dump bytecode */
    status = luaU_dump(H, f, writer_2file, out);
  else
#ifdef HKSC_DECOMPILER
    status = luaU_decompile(H, f, writer_2file, out); /* dump decomp */
#else
    lua_assert(0); /* cannot happen */
#endif /* HKSC_DECOMPILER */
  if (fclose(out)) cannot("close", outname);
  return status;
}
