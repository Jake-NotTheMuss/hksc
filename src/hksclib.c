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


#define CHUNKNAME_BUFFER_SIZE 260

static void cleanfilename(const char *instr, char *outstr) {
  int numdots = 0;
  int length = 0;
  while (*instr != '\0' && length < (CHUNKNAME_BUFFER_SIZE-1)) {
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
  if (!chunkname) chunkname = "?";
  luaZ_init(H, &z, reader, data);
  status = luaD_protectedparser(H, &z, chunkname);
  if (status == 0) lua_assert(H->last_result != NULL);
  return status;
}

static int loadfile(hksc_State *H, const char *filename) {
  char chunknamebuff[CHUNKNAME_BUFFER_SIZE];
  LoadF lf;
  int status, readstatus;
  int c;
  const char *chunkname;
  lf.extraline = 0;
  if (filename == NULL) { /* shouldn't happen */
    hksc_seterror(H, "Hksc does not support reading from stdin");
    return LUA_ERRRUN;
  } else {
    cleanfilename(filename, chunknamebuff);
    chunkname = luaO_generatechunkname(H, chunknamebuff);
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
    lf.f = fopen(filename, "rb");  /* reopen in binary mode */
    if (lf.f == NULL) return errfile(H, "reopen", filename);
    /* skip eventual `#!...' */
    while ((c = getc(lf.f)) != EOF && c != LUA_SIGNATURE[0]) ;
    lf.extraline = 0;
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
  char chunknamebuff[CHUNKNAME_BUFFER_SIZE];
  if (name != buff && name != NULL) {
    if (strlen(name) < (CHUNKNAME_BUFFER_SIZE-1)) {
      char *outstr = chunknamebuff;
      if (*name != '=' && *name != '@')
        *outstr++ = '@';
      cleanfilename(name, outstr);
    }
  }
  ls.s = buff;
  ls.size = size;
  return load(H, getS, &ls, name);
}



/* }====================================================== */


#ifdef LUA_CODT6
#define CYCLE_INFINITE  (-100)

static void resetdebugsource (hksc_State *H) {
  H->debugsource.reader = NULL;
  H->debugsource.ud = NULL;
  H->debugsource.name = NULL;
  H->debugsource.preload = NULL;
  H->debugsource.postload = NULL;
  H->debugsource.cycles = 0;
}
#endif /* LUA_CODT6 */


/* put here all logic to be run at the start of a parser cycle */
static void startcycle(hksc_State *H, const char *name) {
  global_State *g = G(H);
  /* in case ERRORMSG is a string that is about to be collected, reset it to an
     empty string */
  H->errormsg = NULL;
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
#ifdef LUA_CODT6
  if (H->debugsource.cycles != CYCLE_INFINITE) {
    if (--H->debugsource.cycles <= 0)
      resetdebugsource(H);
  }
#endif /* LUA_CODT6 */
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

enum sourcekind_e {
  HKSC_LOAD_FILE,
  HKSC_LOAD_BUFFER,
  HKSC_LOAD_CUSTOM
};

struct SParser {
  int sourcekind;
  size_t size;  /* buffer size if loading from a buffer */
  const char *source;  /* source name */
  lua_Reader reader;
  void *aux;  /* data depending on sourcekind */
  hksc_Dumper dumpf;
  void *ud;
};


static void fparser(hksc_State *H, void *ud) {
  struct SParser *p = (struct SParser *)ud;
  int status;
  startcycle(H, p->source);
  if (p->sourcekind == HKSC_LOAD_FILE)
    status = loadfile(H, p->source);  /* parse file */
  else if (p->sourcekind == HKSC_LOAD_BUFFER)
    status = lua_loadbuffer(H, (const char *)(p->aux), p->size, p->source);
  else
    status = load(H, p->reader, p->aux, p->source);
  if (status) goto fail;
  lua_unlock(H);
  status = (*p->dumpf)(H, p->ud);
  lua_lock(H);
  fail:
  endcycle(H, p->source);
  if (status) luaD_throw(H, status);
}


LUA_API int hksI_parser(hksc_State *H, lua_Reader reader, void *readerdata,
             hksc_Dumper dumpf, void *dumpdata, const char *chunkname)
{
  int status;
  struct SParser p;
  lua_lock(H);
  p.source = chunkname;
  p.reader = reader; p.aux = readerdata;
  p.dumpf = dumpf; p.ud = dumpdata;
  p.size = 0;
  p.sourcekind = HKSC_LOAD_CUSTOM;
  status = luaD_pcall(H, &fparser, &p);
  lua_unlock(H);
  return status;
}


LUA_API int hksI_parser_file(hksc_State *H, const char *filename,
                     hksc_Dumper dumpf, void *ud) {
  int status;
  struct SParser p;
  lua_lock(H);
  p.source = filename;
  p.reader = NULL; p.aux = NULL;
  p.dumpf = dumpf; p.ud = ud;
  p.sourcekind = HKSC_LOAD_FILE;
  status = luaD_pcall(H, &fparser, &p);
  lua_unlock(H);
  return status;
}


LUA_API int hksI_parser_buffer(hksc_State *H, const char *buff, size_t size,
                       const char *source, hksc_Dumper dumpf, void *ud) {
  int status;
  struct SParser p;
  lua_lock(H);
  p.aux = (void *)buff; p.reader = NULL;
  p.size = size; p.source = source;
  p.dumpf = dumpf; p.ud = ud;
  p.sourcekind = HKSC_LOAD_BUFFER;
  status = luaD_pcall(H, &fparser, &p);
  lua_unlock(H);
  return status;
}


#ifdef LUA_CODT6

/*
** LoadDF - differs from LoadF because I delay opening the input file until the
** callback from luaU_undump is called, because until then, it is unknown if
** this file will be used for input or output; if used for output, the file
** shouldn't be opened from inside the library
*/
typedef struct LoadDF {
  const char *filename;
  FILE *f;
  char buff [LUAL_BUFFERSIZE];
} LoadDF;

/* preload callback when the debug info is in a file */
static int opendebugfile (hksc_State *H, const char *chunkname) {
  LoadDF *lf = H->debugsource.ud;
  lua_assert(lf != NULL && lf->f == NULL);
  if (lf->filename == NULL) {
    luaD_setferror(H,"debug file name not set for input `%s'", chunkname);
    return LUA_ERRRUN;
  }
  lf->f = fopen(lf->filename, "rb");
  if (lf->f == NULL)
    return errfile(H, "open", lf->filename);
  return 0;
}

/* postload callback when the debug info is in a file */
static int closedebugfile (hksc_State *H, const char *chunkname) {
  int readstatus, closestatus;
  LoadDF *lf = H->debugsource.ud;
  lua_assert(lf != NULL && lf->f != NULL);
  readstatus = ferror(lf->f);
  closestatus = fclose(lf->f);
  lf->f = NULL;
  if (readstatus) return errfile(H, "read", lf->filename);
  if (closestatus) return errfile(H, "close", lf->filename);
  UNUSED(chunkname);
  return 0;
}


/* reader callback for debug info files */
static const char *getDF (hksc_State *H, void *ud, size_t *size) {
  LoadDF *lf = (LoadDF *)ud;
  (void)H;
  if (feof(lf->f)) return NULL;
  *size = fread(lf->buff, 1, LUAL_BUFFERSIZE, lf->f);
  return (*size > 0) ? lf->buff : NULL;
}


/*
** Debug info reader APIs
*/

LUA_API void hksI_setdebugreader (hksc_State *H, lua_Reader r, void *ud,
                                  const char *name) {
  H->debugsource.reader = r;
  H->debugsource.ud = ud;
  H->debugsource.preload = H->debugsource.postload = NULL;
  /* when the user handles it all on their own, it is expected they will not
     need to change the internal configuration of Hksc every cycle to make
     their system work; on the other hand, if using files for example, they
     would need to specify a new debug info file name every cycle */
  H->debugsource.cycles = CYCLE_INFINITE;
  H->debugsource.name = name ? name : "(debug info source)";
}


LUA_API void hksI_setdebugfile (hksc_State *H, const char *filename) {
  LoadDF *lf;
  if (filename == NULL) {
    resetdebugsource(H);
    return;
  }
  H->debugsource.reader = getDF;
  lf = H->debugsource.ud = luaE_allocdebugsource(H, sizeof(LoadDF));
  H->debugsource.preload = &opendebugfile;
  H->debugsource.postload = &closedebugfile;
  H->debugsource.name = filename;
  H->debugsource.cycles = 1;
  lf->filename = filename;
  lf->f = NULL;
}


LUA_API void hksI_setdebugbuffer (hksc_State *H, const void *buff, size_t size,
                                  const char *name) {
  LoadS *ls;
  H->debugsource.reader = getS;
  H->debugsource.preload = H->debugsource.postload = NULL;
  ls = H->debugsource.ud = luaE_allocdebugsource(H, sizeof(LoadS));
  H->debugsource.name = name ? name : "(debug info string)";
  H->debugsource.cycles = 1;
  ls->s = buff;
  ls->size = size;
}

#endif /* LUA_CODT6 */


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


LUA_API void hksI_compilersettings(hksc_CompilerSettings *settings) {
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
  settings->literals = INT_LITERALS_NONE;
#ifdef HKSC_DECOMPILER
  settings->match_line_info = 1;
#endif /* HKSC_DECOMPILER */
}

LUA_API void hksI_settings(hksc_StateSettings *settings) {
  hksI_compilersettings(&settings->compilersettings);
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
    hksI_settings(&default_settings);
    settings = &default_settings;
  }
  H = lua_newstate(settings);
  return H;
}

LUA_API void hksI_close(hksc_State *H) {
  lua_close(H);
}

