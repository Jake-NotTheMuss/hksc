/*
** $Id: hkscaux.c $
** Dumping bytecode to files
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <ctype.h>

#define hkscaux_c

#include "hksclua.h"
#include "hkscluaconf.h"

#include "hksclib.h"

#include "hkscaux.h"

extern const struct Opts opts;

#ifdef LUA_CODT6
const char *debug_file;
const char *callstack_file;
#endif /* LUA_CODT6 */
/*extern const char *debugext;*/

/* default extension of pre-compiled Lua files */
#define LUAC_EXT  ".luac"
/* default extension of source Lua files */
#define LUA_EXT  ".lua"
/* default extension of decompiled Lua files */
#define LUADEC_EXT  ".dec.lua"

#ifdef LUA_CODT6
/* default extension of callstack reconstruction files */
# define LUACALLSTACK_EXT  ".luacallstackdb"
/* defalt extension of debug info files */
# define LUADEBUG_EXT  ".luadebug"
#else /* !LUA_CODT6 */
# define LUACALLSTACK_EXT  ".luacprofile"
# define LUADEBUG_EXT  ".luacdebug"
#endif /* LUA_CODT6 */


#define hasluaext(s,l) (hasext(s,l,LUA_EXT))
#define hasluacext(s,l) (hasext(s,l,LUAC_EXT))


#define MAXEXT 32 /* arbitrary limit on the length of file extensions */

/* returns whether a file name has a given extension (C89, case-insensative) */
static int hasext(const char *name, size_t namelen, const char *ext) {
  char buff[MAXEXT];
  size_t n;
  size_t extlen;
  extlen = strlen(ext);
  if (namelen < extlen || extlen < 2) return 0;
  for (n = 0; n < extlen; n++)
    buff[n]=tolower(name[namelen-(extlen-n)]);
  buff[n]='\0';
  return (strcmp(buff, ext) == 0);
}

#define lua2luac(buff,name) lua2ext(buff,name,LUAC_EXT)
#define luac2luadec(buff,name) lua2ext(buff,name,LUADEC_EXT)
#define lua2luaprofile(buff,name) lua2ext(buff,name,LUACALLSTACK_EXT)
#define lua2luadebug(buff,name) lua2ext(buff,name,LUADEBUG_EXT)

#define MAXLUACNAME 1024


#if (defined(_WIN32) && !defined(__CYGWIN__)) || defined(__MSDOS__) || \
  defined(__DJGPP__) || defined(__OS2__)
# define HAVE_DOS_BASED_FILE_SYSTEM
# define IS_DIR_SEP(c)  ((c) == '/' || (c) == '\\')
#else
# undef HAVE_DOS_BASED_FILE_SYSTEM
# define IS_DIR_SEP(c)  ((c) == '/') 
#endif

static const char *basename(const char *name) {
  const char *base;
#ifdef HAVE_DOS_BASED_FILE_SYSTEM
  /* skip over disk name in MSDOS pathnames */
  if (isalpha(name[0]) && name[1] == ':')
    name += 2;
#endif /* HAVE_DOS_BASED_FILE_SYSTEM */
  for (base = name; *name; name++) {
    if (IS_DIR_SEP(*name))
      base = name + 1;
  }
  return base;
}


static char outname_buff [MAXLUACNAME];


/*
** generate a name for a an output file from a given input name
*/
static char *lua2ext(char buff [MAXLUACNAME], const char *name,
                     const char *ext) {
  size_t n = 0;
  size_t namelen = strlen(name);
  size_t extlen = strlen(ext);
  if (hasluacext(name,namelen) && (strcmp(ext,LUAC_EXT)!=0)) /* `*.luac'? */
    namelen-=sizeof(LUAC_EXT)-1; /* remove `.luac' */
  else if (hasluaext(name,namelen) && (strcmp(ext,LUA_EXT)!=0)) /* `*.lua'? */
    namelen-=sizeof(LUA_EXT)-1; /* remove `.lua' */
  if ((namelen+extlen+1) >= MAXLUACNAME)
    namelen = MAXLUACNAME-(extlen+1);
  memcpy(buff, name, namelen);
  n+=namelen;
  memcpy(buff+n, ext, extlen);
  n+=extlen;
  buff[n]='\0';
  return buff;
}

static int writer_2file(hksc_State *H, const void *p, size_t size, void *u) {
  size_t n;
  (void)H;
  n = fwrite(p,size,1,(FILE*)u);
#ifdef LUA_DEBUG
  fflush((FILE*)u);
#endif /* LUA_DEBUG */
  return (n!=1) && (size!=0);
}

/* push error string "cannot <what> <filename>" */
#define cannot(what,name) do { \
  char buffer [1024]; \
  sprintf(buffer, "cannot " what " %.512s: %.64s", name, strerror(errno)); \
  lua_seterror(H, buffer); \
  return LUA_ERRFILE; \
} while (0)

#define xopenfile(var,name,mode) do { \
  var = fopen(name,mode); \
  if (var==NULL) cannot("open",name); \
} while (0)

#ifdef LUA_CODT6

static char debug_file_buff [MAXLUACNAME];
static char callstack_file_buff [MAXLUACNAME];

void luacod_startcycle(hksc_State *H, const char *name) {
  /* err on the side of generating names from the input file - output debug
     names are not needed until the dump stage, whereas input debug names are
     needed from the start; if dumping, the names will be regenerated to go to
     the output directory; this doesn't apply when the names are provided
     explicitly in the command line  */
  if (lua_getmode(H) == HKSC_MODE_SOURCE && opts.output != NULL)
    name = opts.output;/* the debug files go the directory with the output
    file */
  else if (name == NULL)
    name = STDIN_NAME;
  if (opts.with_debug) {
    debug_file = opts.debug_file;
    callstack_file = opts.callstack_file;
    if (debug_file == NULL) /* may be provided in command line */
      debug_file = lua2luadebug(debug_file_buff, name);
    if (callstack_file == NULL)
      callstack_file = lua2luaprofile(callstack_file_buff, name);
    hksI_setdebugfile(H, debug_file);
  }
}

void luacod_endcycle(hksc_State *H, const char *name) {
  (void)H; (void)name;
  /* output names are only generated if they were previously NULL; this works
     even if output names were explicitly provided, since only 1 cycle should
     run in that case */
  debug_file = NULL;
  callstack_file = NULL;
}

#define dumpdebugfile(name, striplevel, mode) do { \
  FILE *debug_file_; \
  xopenfile(debug_file_,name,mode); \
  lua_setstrip(H,striplevel); \
  lua_dump(H,writer_2file,debug_file_); \
  if (ferror(debug_file_)) cannot("write",name); \
  if (fclose(debug_file_)) cannot("close",name); \
} while (0)


/* (COD) dump debug info to files */
static int luacod_dumpdebug(hksc_State *H, const char *outname){
  /* make sure generated names are in the same directory as outname */
  if (opts.with_debug) {
    if (debug_file == NULL)
      debug_file = lua2luadebug(debug_file_buff, outname);
    dumpdebugfile(debug_file, BYTECODE_STRIPPING_DEBUG_ONLY, "wb");
    if (callstack_file == NULL)
      callstack_file = lua2luaprofile(callstack_file_buff, outname);
    dumpdebugfile(callstack_file, BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION,
                  "w");
  }
  lua_setstrip(H,BYTECODE_STRIPPING_ALL); /* reset */
  return 0;
}
#endif /* LUA_CODT6 */


int hksc_list_bytecode(hksc_State *H, const char *filename, int full) {
  int status;
  FILE *f = filename ? fopen(filename, "w") : stdout;
  if (f == NULL)
    cannot("open", filename);
  status = lua_print(H, writer_2file, f, full);
  if (filename != NULL && fclose(f))
    cannot("close", filename);
  return status;
}


int hksc_dump_bytecode(hksc_State *H, const char *filename) {
  int status;
  FILE *out; /* output file */
  const char *outname; /* output file name */
  if (opts.output == NULL) /* generate an output name if needed */
    outname = basename(lua2luac(outname_buff, filename));
  else
    outname = opts.output;
#ifdef LUA_CODT6
  status = luacod_dumpdebug(H, outname);/* dump debug info to separate files */
  if (status)
    return status;
#endif /* LUA_CODT6 */
  out = fopen(outname, "wb");
  if (out == NULL)
    cannot("open", outname);
  status = lua_dump(H, writer_2file, out);
  if (fclose(out))
    cannot("close", outname);
  return status;
}


#ifdef HKSC_DECOMPILER

int hksc_dump_decomp(hksc_State *H, const char *filename) {
  int status;
  FILE *out; /* output file */
  const char *outname; /* output file name */
  if (opts.output == NULL) /* generate an output name if needed */
    outname = basename(luac2luadec(outname_buff, filename));
  else
    outname = opts.output;
  out = fopen(outname, "w");
  if (out == NULL)
    cannot("open", outname);
  status = lua_decompile(H, writer_2file, out);
  if (fclose(out))
    cannot("close", outname);
  return status;
}

#endif /* HKSC_DECOMPILER */
