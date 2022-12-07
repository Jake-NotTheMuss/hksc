/*
** $Id: hksclib.c $
** Protective wrapper around Lua library calls
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

#define hksclib_c
#define LUA_CORE

#include "luaconf.h"
#include "lua.h"

#include "hksclib.h"

#include "lctype.h"
#include "ldo.h"
#include "lgc.h"
#include "lobject.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "lundump.h"
#include "lzio.h"


/*
** assert that that Hksc is running in a given mode
*/
#define checkmode(H,m,f) do { \
  if (checkmode_(H,m,f)) return LUA_ERRRUN; \
} while (0)

#ifdef HKSC_DECOMPILER
#define checkmodematches(H,c,f) do { \
  if ((c) == LUA_SIGNATURE[0]) \
    checkmode(H,HKSC_MODE_DECOMPILE,f); \
  else \
    checkmode(H,HKSC_MODE_COMPILE,f); \
} while (0)
#else /* !HKSC_DECOMPILER */
#define checkmodematches(H,c,f) do { \
  if ((c) == LUA_SIGNATURE[0]) { \
    hksc_setfmsg((H), "`%s' is already a pre-compiled Lua file", (f)); \
    return LUA_ERRRUN; \
  } \
  checkmode((H),HKSC_MODE_COMPILE,(f)); \
} while (0)
#endif /* HKSC_DECOMPILER */

static int checkmode_(hksc_State *H, int mode, const char *filename) {
  lua_assert(mode != HKSC_MODE_DEFAULT);
  if (luaE_mode(H) == HKSC_MODE_DEFAULT)
    luaE_mode(H) = mode; /* set the mode if it wasn't set */
  else if (luaE_mode(H) != mode) {
    if (luaE_mode(H) == HKSC_MODE_COMPILE)
      hksc_setfmsg(H, "cannot process `%s' in compile-mode, it is already a "
        "pre-compiled Lua file", filename);
    else
      hksc_setfmsg(H, "cannot process `%s' in decompile-mode, it is not a "
        "pre-compiled Lua file", filename);
    return 1;
  }
  return 0;
}

static void cleanFileName(char *instr, char *outstr) {
  int numdots = 0;
  int length = 0;
  while (*instr != '\0' && length < (PATH_MAX-1)) {
    if (*instr == '.')
      numdots++;
    else if (numdots == 1 && (*instr == '/' || *instr == '\\')) /* omit `./' */
      numdots = 0;
    else {
      for (; numdots > 0; numdots--)
        *outstr++ = '.';
      *outstr++ = *instr;
    }
    instr++;
    length++;
  }
  *outstr = '\0';
}


/*
** {======================================================
** Load functions
** =======================================================
*/

typedef struct LoadF
{
  int extraline;
  FILE *f;
  char buff[LUAL_BUFFERSIZE];
} LoadF;


static const char *getF (hksc_State *H, void *ud, size_t *size) {
  LoadF *lf = (LoadF *)ud;
  (void)H;
  if (lf->extraline) {
    lf->extraline = 0;
    *size = 1;
    return "\n";
  }
  if (feof(lf->f)) return NULL;
  *size = fread(lf->buff, 1, LUAL_BUFFERSIZE, lf->f);
  return (*size > 0) ? lf->buff : NULL;
}

static int errfile (hksc_State *H, const char *what, const char *filename) {
  const char *serr = strerror(errno);
  hksc_setfmsg(H, "cannot %s %s: %s", what, filename, serr);
  return LUA_ERRFILE;
}


static int load(hksc_State *H, lua_Reader reader, void *data,
                const char *chunkname) {
  ZIO z;
  int status;
  lua_lock(H);
  lua_assert(luaE_mode(H) != HKSC_MODE_DEFAULT); /* the mode must be known */
  if (!chunkname) chunkname = "?";
  luaZ_init(H, &z, reader, data);
  status = luaD_protectedparser(H, &z, chunkname);
  if (status == 0) lua_assert(H->last_result != NULL);
  lua_unlock(H);
  return status;
}

static int loadfile(hksc_State *H, const char *filename) {
  LoadF lf;
  int status, readstatus;
  int c;
  const char *chunkname;
  lf.extraline = 0;
  if (filename == NULL) { /* shouldn't happen */
    hksc_setliteralmsg(H, "Hksc does not support reading from stdin");
    return LUA_ERRRUN;
  } else {
    chunkname = luaO_pushfstring(H, "@%s", filename);
    lf.f = fopen(filename, "r");
    if (lf.f == NULL) return errfile(H, "open", filename);
  }
  c = getc(lf.f);
  if (c == '#') {  /* Unix exec. file? */
    lf.extraline = 1;
    while ((c = getc(lf.f)) != EOF && c != '\n') ; /* skip first line */
    if (c == '\n') c = getc(lf.f);
  }
  if (c == LUA_SIGNATURE[0] && lf.f != stdin) {  /* binary file? */
    fclose(lf.f);
#ifndef HKSC_DECOMPILER /* built without a decompiler */
    hksc_setliteralmsg(H, "Attempt to compile a binary Lua file, source file "
                       "expected");
    return LUA_ERRRUN;
#else /* HKSC_DECOMPILER */
    /* make sure this state is in decompile-mode */
    checkmode(H, HKSC_MODE_DECOMPILE, filename);
    lf.f = fopen(filename, "rb");  /* reopen in binary mode */
    if (lf.f == NULL) return errfile(H, "reopen", filename);
    /* skip eventual `#!...' */
    while ((c = getc(lf.f)) != EOF && c != LUA_SIGNATURE[0]) ;
    lf.extraline = 0;
#endif /* !HKSC_DECOMPILER */
  } else { /* source file */
    /* make sure this state is in compile-mode */
    checkmode(H, HKSC_MODE_COMPILE, filename);
  }
  ungetc(c, lf.f);
  status = load(H, getF, &lf, chunkname);
  readstatus = ferror(lf.f);
  if (lf.f != stdin) fclose(lf.f);  /* close file (even in case of errors) */
  if (readstatus)
    return errfile(H, "read", filename);
  return status;
}


typedef struct LoadS {
  const char *s;
  size_t size;
} LoadS;


static const char *getS (hksc_State *H, void *ud, size_t *size) {
  LoadS *ls = (LoadS *)ud;
  (void)H;
  if (ls->size == 0) return NULL;
  *size = ls->size;
  ls->size = 0;
  return ls->s;
}

static int lua_loadbuffer (hksc_State *H, const char *buff, size_t size,
                           char *name) {
  LoadS ls;
  char cleanedFileName[PATH_MAX];
  if (name != buff && name != NULL) {
    if (strlen(name) < (PATH_MAX-1)) {
      char *outstr;
      if (*name != '=' && *name != '@') {
        cleanedFileName[0] = '@';
        outstr = &cleanedFileName[1];
      } else
        outstr = &cleanedFileName[0];
      cleanFileName(name, outstr);
    }
  }
  checkmodematches(H, *buff, name);
  ls.s = buff;
  ls.size = size;
  return load(H, getS, &ls, name);
}



/* }====================================================== */


/* put here all logic to be run at the start of a parser cycle */
static void startcycle(hksc_State *H, const char *name) {
  lua_assert(G(H)->gcstate == GCSpause); /* GC only active between cycles */
  luaC_newcycle(H); /* collect garbage */
  /* end of library start-cycle logic */
  if (G(H)->startcycle) /* user-defined logic */
    (*G(H)->startcycle)(H, name);
}


/* put here all logic to be run at the end of a parser cycle */
static void endcycle(hksc_State *H, const char *name) {
  lua_assert(G(H)->gcstate == GCSpause); /* GC only active between cycles */
  if (G(H)->endcycle) /* user-defined logic */
    (*G(H)->endcycle)(H, name);
  /* start of library end-cycle logic */
  H->last_result = NULL; /* will be collected, remove dangling pointer */
}


/*
** {======================================================
** Parser functions
** =======================================================
*/


int hksI_parser_file(hksc_State *H, const char *filename,
                     hksc_DumpFunction dumpf, void *ud) {
  int status;
  startcycle(H, filename);
  status = loadfile(H, filename); /* parse file */
  if (status) goto fail;
  status = (*dumpf)(H, H->last_result, ud);
  fail:
  endcycle(H, filename);
  return status;
}


int hksI_parser_buffer(hksc_State *H, const char *buff, size_t size,
                       char *source, hksc_DumpFunction dumpf, void *ud) {
  int status;
  startcycle(H, source);
  status = lua_loadbuffer(H, buff, size, source);
  if (status) goto fail;
  status = (*dumpf)(H, H->last_result, ud);
  fail:
  endcycle(H, source);
  return status;
}


static void *l_alloc (void *ud, void *ptr, size_t osize, size_t nsize) {
  (void)ud;
  (void)osize;
  if (nsize == 0) {
    free(ptr);
    return NULL;
  }
  else
    return realloc(ptr, nsize);
}

static int panic (hksc_State *H) {
  (void)H;  /* to avoid warnings */
  fprintf(stderr, "PANIC: unprotected error in call to Lua API (%s)\n",
                   luaE_geterrormsg(H));
  return 0;
}

hksc_State *hksI_newstate(int mode) {
  hksc_State *H = hksc_newstate(l_alloc, NULL);
  if (H) {
    hksc_atpanic(H, &panic);
    luaE_mode(H) = mode;
  }
  return H;
}

void hksI_close(hksc_State *H) {
  hksc_close(H);
}

