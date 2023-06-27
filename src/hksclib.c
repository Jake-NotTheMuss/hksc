/*
** $Id: hksclib.c $
** Protective wrapper around Lua library calls
** See Copyright Notice in lua.h
*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

#define hksclib_c
#define LUA_CORE

#include "hkscluaconf.h"
#include "hksclua.h"

#include "hksclib.h"

#include "ldo.h"
#include "ldebug.h"
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


#define checkmodematches(H,c,f) do { \
  if ((c) == LUA_SIGNATURE[0]) \
    checkmode(H,HKSC_MODE_BINARY,f); \
  else \
    checkmode(H,HKSC_MODE_SOURCE,f); \
} while (0)


static int checkmode_(hksc_State *H, int mode, const char *filename) {
  int oldmode = hksc_mode(H);
  lua_assert(mode != HKSC_MODE_DEFAULT);
  if (oldmode == HKSC_MODE_DEFAULT)
    hksc_mode(H) = mode; /* set the mode if it wasn't set */
  else if (oldmode != mode) {
    if (oldmode == HKSC_MODE_SOURCE)
      luaD_setferror(H, "cannot process `%s' in compile-mode, it is already a "
        "pre-compiled Lua file", filename);
    else
      luaD_setferror(H, "cannot process `%s' in decompile-mode, it is not a "
        "pre-compiled Lua file", filename);
    return 1;
  }
  return 0;
}


#define MAX_CLEANED_FILENAME_SIZE 260

static void cleanFileName(const char *instr, char *outstr) {
  int numdots = 0;
  int length = 0;
  while (*instr != '\0' && length < (MAX_CLEANED_FILENAME_SIZE-1)) {
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
  luaD_setferror(H, "cannot %s %s: %s", what, filename, serr);
  return LUA_ERRFILE;
}


static int load(hksc_State *H, lua_Reader reader, void *data,
                const char *chunkname) {
  ZIO z;
  int status;
  lua_assert(hksc_mode(H) != HKSC_MODE_DEFAULT); /* the mode must be known */
  if (!chunkname) chunkname = "?";
  luaZ_init(H, &z, reader, data);
  status = luaD_protectedparser(H, &z, chunkname);
  if (status == 0) lua_assert(H->last_result != NULL);
  return status;
}

static int loadfile(hksc_State *H, const char *filename) {
  char cleanedFileName[MAX_CLEANED_FILENAME_SIZE];
  LoadF lf;
  int status, readstatus;
  int c;
  const char *chunkname;
  lf.extraline = 0;
  if (filename == NULL) { /* shouldn't happen */
    hksc_seterror(H, "Hksc does not support reading from stdin");
    return LUA_ERRRUN;
  } else {
    cleanFileName(filename, cleanedFileName);
    chunkname = luaO_generatechunkname(H, cleanedFileName);
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
    /* make sure this state is in decompile-mode */
    checkmode(H, HKSC_MODE_BINARY, filename);
    lf.f = fopen(filename, "rb");  /* reopen in binary mode */
    if (lf.f == NULL) return errfile(H, "reopen", filename);
    /* skip eventual `#!...' */
    while ((c = getc(lf.f)) != EOF && c != LUA_SIGNATURE[0]) ;
    lf.extraline = 0;
  } else { /* source file */
    /* make sure this state is in compile-mode */
    checkmode(H, HKSC_MODE_SOURCE, filename);
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
                           const char *name) {
  LoadS ls;
  char cleanedFileName[MAX_CLEANED_FILENAME_SIZE];
  if (name != buff && name != NULL) {
    if (strlen(name) < (MAX_CLEANED_FILENAME_SIZE-1)) {
      char *cleanedFileNamePtr;
      if (*name != '=' && *name != '@') {
        cleanedFileName[0] = '@';
        cleanedFileNamePtr = &cleanedFileName[1];
      } else
        cleanedFileNamePtr = &cleanedFileName[0];
      cleanFileName(name, cleanedFileNamePtr);
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
  global_State *g = G(H);
#ifdef LUA_DEBUG
  if (g->incyclecallback) {
    luaG_runerror(H, "cannot start a new parser cycle from inside a "
                  "start-cycle callback");
    return;
  }
#endif /* LUA_DEBUG */
  H->currinputname = name;
  lua_assert(H->last_result == NULL);
  luaC_newcycle(H); /* collect garbage while in-between cycles */
  g->midcycle = 1;
  /* end of library start-cycle logic */
  if (G(H)->startcycle) { /* user-defined logic */
#ifdef LUA_DEBUG
    g->incyclecallback = 1;
#endif /* LUA_DEBUG */
    lua_unlock(H);
    (*G(H)->startcycle)(H, name);
    lua_lock(H);
#ifdef LUA_DEBUG
    g->incyclecallback = 0;
#endif /* LUA_DEBUG */
  }
  lua_assert(!g->incyclecallback);
}


/* put here all logic to be run at the end of a parser cycle */
static void endcycle(hksc_State *H, const char *name) {
  global_State *g = G(H);
  lua_assert(g->midcycle);
  lua_assert(!g->incyclecallback);
  g->midcycle = 0; /* set this before running user-defined logic so that temp
                      objects can be collected if a GC cycle happens */
  if (H->last_result != NULL) /* it can be NULL if there was a syntax error */
    /* make sure the compiler result is not collected until the cycle really
       ends */
    makelive(obj2gco(H->last_result));
  H->currinputname = NULL;
  if (G(H)->endcycle) { /* user-defined logic */
#ifdef LUA_DEBUG
    g->incyclecallback = 1;
#endif /* LUA_DEBUG */
    lua_unlock(H);
    (*G(H)->endcycle)(H, name);
    lua_lock(H);
#ifdef LUA_DEBUG
    g->incyclecallback = 0;
#endif /* LUA_DEBUG */
  }
  /* start of library end-cycle logic */
  if (H->last_result != NULL)
    makedead(obj2gco(H->last_result)); /* now it can die */
  H->last_result = NULL; /* will be collected, remove dangling pointer */
}


/*
** {======================================================
** Parser functions
** =======================================================
*/

struct SParser {
  union {
    const char *filename;
    const char *buff;
  } arg1;
  size_t size;
  const char *source;
  hksc_DumpFunction dumpf;
  void *ud;
};

static void fparser_file(hksc_State *H, void *ud) {
  struct SParser *p = (struct SParser *)ud;
  int status;
  startcycle(H, p->arg1.filename);
  status = loadfile(H, p->arg1.filename); /* parse file */
  if (status) goto fail;
  lua_unlock(H);
  status = (*p->dumpf)(H, p->ud);
  lua_lock(H);
  fail:
  endcycle(H, p->arg1.filename);
  if (status) luaD_throw(H, status);
}

static void fparser_buffer(hksc_State *H, void *ud) {
  struct SParser *p = (struct SParser *)ud;
  int status;
  startcycle(H, p->source);
  status = lua_loadbuffer(H, p->arg1.buff, p->size, p->source);
  if (status) goto fail;
  lua_unlock(H);
  status = (*p->dumpf)(H, p->ud);
  lua_lock(H);
  fail:
  endcycle(H, p->source);
  if (status) luaD_throw(H, status);
}


LUA_API int hksI_parser_file(hksc_State *H, const char *filename,
                     hksc_DumpFunction dumpf, void *ud) {
  int status;
  struct SParser p;
  lua_lock(H);
  p.arg1.filename = filename;
  p.dumpf = dumpf; p.ud = ud;
  status = luaD_pcall(H, &fparser_file, &p);
  lua_unlock(H);
  return status;
}


LUA_API int hksI_parser_buffer(hksc_State *H, const char *buff, size_t size,
                       const char *source, hksc_DumpFunction dumpf, void *ud) {
  int status;
  struct SParser p;
  lua_lock(H);
  p.arg1.buff = buff;
  p.size = size; p.source = source;
  p.dumpf = dumpf; p.ud = ud;
  status = luaD_pcall(H, &fparser_buffer, &p);
  lua_unlock(H);
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
                   lua_geterror(H));
  return 0;
}

#if defined(LUA_CODT6) /*&& defined(HKSC_DECOMPILER)*/

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


static int init_debug_reader(hksc_State *H, ZIO *z, Mbuffer *buff,
                             const char *name) {
  LoadDebug *ld;
  FILE *f;
  lua_assert(hksc_getignoredebug(H) == 0);
  if (H->currdebugfile == NULL) {
    luaD_setferror(H, "debug file name not set for input `%s'", name);
    return LUA_ERRRUN;
  }
  ld = luaM_new(H, LoadDebug);
  f = fopen(H->currdebugfile, "rb");
  if (f == NULL) {
    luaM_free(H, ld);
    return errfile(H, "open", H->currdebugfile);
  }
  ld->f = f;
  luaZ_init(H, z, debug_reader, ld);
  luaZ_initbuffer(H, buff);
  return 0;
}

static int close_debug_reader(hksc_State *H, ZIO *z, Mbuffer *buff,
                              const char *name) {
  int readstatus, closestatus;
  LoadDebug *ld;
  FILE *f;
  UNUSED(name);
  lua_assert(hksc_getignoredebug(H) == 0);
  ld = z->data;
  f = ld->f;
  luaM_free(H, ld); UNUSED(ld);
  luaZ_freebuffer(H, buff); UNUSED(buff);
  if (f == NULL) return 0;
  readstatus = ferror(f);
  closestatus = fclose(f);
  UNUSED(f);
  if (readstatus) return errfile(H, "read", H->currdebugfile);
  if (closestatus) return errfile(H, "close", H->currdebugfile);
  return 0;
}

#endif /* defined(LUA_CODT6) && defined(HKSC_DECOMPILER) */

LUA_API void hksI_CompilerSettings(hksc_CompilerSettings *settings) {
#ifdef LUA_CODT6
# ifdef LUA_CODT7
  settings->hash_step = 1;
# else /* !LUA_CODT7 */
  settings->hash_step = 2;
# endif /* LUA_CODT7 */
#endif /* LUA_CODT6 */
  settings->ignore_debug = 0;
  settings->emit_struct = HKSC_STRUCTURE_EXTENSION_ON;
#if HKSC_GETGLOBAL_MEMOIZATION
  settings->emit_memo = 1;
  settings->skip_memo = 0;
#endif /* HKSC_GETGLOBAL_MEMOIZATION */
  settings->enable_int_literals = INT_LITERALS_NONE;
#ifdef HKSC_DECOMPILER
  settings->match_line_info = 1;
#endif /* HKSC_DECOMPILER */
}

LUA_API void hksI_StateSettings(hksc_StateSettings *settings) {
  hksI_CompilerSettings(&settings->compilersettings);
  settings->frealloc = &l_alloc;
  settings->ud = NULL;
  settings->panic = &panic;
  settings->mode = HKSC_MODE_DEFAULT;
  settings->bytecode_endianness = HKSC_DEFAULT_ENDIAN;
#ifdef HKSC_MULTIPLAT
  settings->target_plat = HKSC_TARGET_PLAT_DEFAULT;
  settings->target_ws = HKSC_TARGET_WS_DEFAULT;
#endif /* HKSC_MULTIPLAT */
}


LUA_API hksc_State *hksI_newstate(hksc_StateSettings *settings)
{
  hksc_State *H;
  hksc_StateSettings default_settings;
  if (!settings) {
    hksI_StateSettings(&default_settings);
    settings = &default_settings;
  }
  H = lua_newstate(settings);
  if (H) {
#if defined(LUA_CODT6)
    /* Call of Duty needs a separate debug reader when loading bytecode */
    G(H)->debugLoadStateOpen = init_debug_reader;
    G(H)->debugLoadStateClose = close_debug_reader;
#endif /* LUA_CODT6 */
  }
  return H;
}

LUA_API void hksI_close(hksc_State *H) {
  lua_close(H);
}

