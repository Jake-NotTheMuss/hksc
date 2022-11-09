#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>

#include "hksclib.h"

#define LUA_CORE

#include "luaconf.h"
#include "lua.h"

#include "lctype.h"
#include "ldo.h"
#include "lgc.h"
#include "lobject.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "lundump.h"
#include "lzio.h"



#define hasluaext(n,l) (hasext(n,l,LUA_EXT))
#define hasluacext(n,l) (hasext(n,l,LUAC_EXT))

#define MAXEXT 32 /* arbitrary limit on the length of file extensions */

static int hasext(const char *name, size_t namelen, const char *ext) {
  char buf[MAXEXT];
  size_t len;
  size_t extlen = strlen(ext);
  lua_assert(extlen < MAXEXT);
  if (namelen < extlen || extlen < 2) return 0;
  for (len = 0; len < extlen; len++)
    buf[len]=ltolower(name[namelen-(extlen-len)]);
  buf[len]='\0';
  return (strcmp(buf, ext) == 0);
}

#define MAXLUACNAME 512

/*
** generate a name for a new compiled Lua file
*/
static const char *lua2luac(hksc_State *H, const char *name) {
  char buf[MAXLUACNAME];
  size_t len = 0;
  size_t namelen = strlen(name);
  if (hasluaext(name,namelen) && namelen < (MAXLUACNAME-1)) {
    /* already has lua extension, just append `c' to the name */
    memcpy(buf, name, namelen);
    len+=namelen;
    buf[len++]=(name[namelen-1] == 'A') ? 'C' : 'c';
  } else {
    if ((namelen+sizeof(LUAC_EXT)) >= MAXLUACNAME)
      namelen = MAXLUACNAME-1-sizeof(LUAC_EXT);
    memcpy(buf, name, namelen);
    len+=namelen;
    memcpy(buf+len, LUAC_EXT, sizeof(LUAC_EXT));
    len+=sizeof(LUAC_EXT);
  }
  buf[len]='\0';
  return getstr(luaS_newlstr(H, buf, len));
}


/*
** generate a name for a new decompiled Lua file
*/
static const char *luac2luadec(hksc_State *H, const char *name) {
  char buf[MAXLUACNAME];
  size_t len = 0;
  size_t namelen = strlen(name);
  if (hasluacext(name,namelen)) /* `*.luac'? */
    namelen-=sizeof(LUAC_EXT); /* remove `.luac' */
  else if (hasluaext(name,namelen)) /* `*.lua'? */
    namelen-=sizeof(LUA_EXT); /* remove `.lua' */
  if ((namelen+sizeof(LUADEC_EXT)) >= MAXLUACNAME)
    namelen = MAXLUACNAME-1-sizeof(LUADEC_EXT);
  memcpy(buf, name, namelen);
  len+=namelen;
  memcpy(buf+len, LUADEC_EXT, sizeof(LUADEC_EXT));
  len+=sizeof(LUADEC_EXT);
  buf[len]='\0';
  return getstr(luaS_newlstr(H, buf, len));
}

/*
** assert that that Hksc is running in a given mode
*/
#define checkmode(H,m,f) do { \
  int mode_status_ = checkmode_(H,m,f); \
  if (mode_status_) return mode_status_; } while (0)

static int checkmode_(hksc_State *H, int mode, const char *filename) {
  lua_assert(mode != HKSC_MODE_DEFAULT);
  if (luaE_mode(H) == HKSC_MODE_DEFAULT)
    luaE_mode(H) = mode; /* set the mode if it wasn't set */
  else if (luaE_mode(H) != mode) {
    if (luaE_mode(H) == HKSC_MODE_COMPILE)
      hksc_setfmsg(H, "cannot process `%s' in compile-mode, it is already "
        "pre-compiled\n  --> (luaE_mode(H) == HKSC_MODE_COMPILE)", filename);
    else
      hksc_setfmsg(H, "cannot process `%s' in decompile-mode, it is not a "
        "pre-compiled Lua file\n  --> (luaE_mode(H) == HKSC_MODE_DECOMPILE)",
        filename);
    return LUA_ERRRUN;
  }
  return 0;
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
  lua_assert(luaE_mode(H) != HKSC_MODE_DEFAULT); /* the mode must be known */
  lua_lock(H);
  if (!chunkname) chunkname = "?";
  luaZ_init(H, &z, reader, data);
  status = luaD_protectedparser(H, &z, chunkname);
  lua_unlock(H);
  if (!status)
    lua_assert(H->last_result != NULL);
  return status;
}

static int loadfile(hksc_State *H, const char *filename) {
  LoadF lf;
  const char *chunkname;
  int status, readstatus;
  int c;
  lf.extraline = 0;
  if (filename == NULL) {
    hksc_setliteralmsg(H, "NULL filename supplied to hksI_parsefile()");
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
    /* make sure this state is in decompile-mode */
    checkmode(H, HKSC_MODE_DECOMPILE, filename);
    lf.f = fopen(filename, "rb");  /* reopen in binary mode */
    if (lf.f == NULL) return errfile(H, "reopen", filename);
    /* skip eventual `#!...' */
    while ((c = getc(lf.f)) != EOF && c != LUA_SIGNATURE[0]) {}
      lf.extraline = 0;
  } else { /* source file */
    /* make sure this state is in compile-mode */
    checkmode(H, HKSC_MODE_COMPILE, filename);
  }
  ungetc(c, lf.f);
  status = load(H, getF, &lf, chunkname);
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


static int loadbuffer (hksc_State *H, const char *buff, size_t size,
                       const char *name) {
  LoadS ls;
  if (name == NULL || *name == '\0') {
    hksc_setliteralmsg(H, "NULL or empty chunk name supplied to loadbuffer");
    return LUA_ERRRUN;
  }
  if (buff == NULL) {
    hksc_setfmsg(H, "NULL buffer supplied to loadbuffer (when loading "
      "named chunk `%s')", name);
    return LUA_ERRRUN;
  }
  if (*buff == LUA_SIGNATURE[0])
    checkmode(H, HKSC_MODE_DECOMPILE, name);
  else
    checkmode(H, HKSC_MODE_COMPILE, name);
  ls.s = buff;
  ls.size = size;
  return load(H, getS, &ls, name);
}



/* }====================================================== */


#define startcycle(H) do { \
  global_State *g_ = G(H); \
  lua_assert(g_->gcstate == GCSpause); /* GC only occurs between operations */ \
  luaC_newcycle(H); /* collect old prototypes and tables */ \
  luaC_checkGC(H); /* maybe collect garbage */ \
  } while (0)


#define endcycle(H) do { \
  (H)->last_result = NULL; /* will be collected, remove dangling pointer */ \
  } while (0)


static int writer_2file(hksc_State *H, const void *p, size_t sz, void *ud) {
  UNUSED(H);
  FILE *out = (FILE *)ud;
  if (fwrite(p, 1, sz, out) != sz)
    return 1;
  else
    return 0;
}


typedef struct WriteS {
  char *buf;
  size_t size; /* toatal size */
  size_t pos; /* number of bytes written */
} WriteS;


static int writer_2buf(hksc_State *H, const void *p, size_t sz, void *ud) {
  UNUSED(H);
  WriteS *ws = (WriteS *)ud;
  memcpy(ws->buf + ws->pos, p, sz);
  ws->pos += sz;
  return 0;
}


static int dump2file(hksc_State *H, const char *inname, const char *outname) {
  int status;
  Proto *f;
  FILE *out;
  f = H->last_result;
  if (f == NULL) return H->status; /* error during parsing, do not dump */
  if (outname == NULL) { /* generate an output name if needed */
    if (luaE_mode(H) == HKSC_MODE_COMPILE)
      outname = lua2luac(H, inname);
    else
      outname = luac2luadec(H, inname);
  }
  lua_assert(outname != NULL);
  out = fopen(outname, "wb");
  if (out == NULL) return errfile(H, "open", outname);
  status = luaU_dump(H, f, writer_2file, out);
  fclose(out);
  return status;
}


static int dump2buf(hksc_State *H, char *buf, size_t size) {
  int status;
  WriteS ws;
  Proto *f = H->last_result;
  if (f == NULL) return H->status; /* error during parsing, do not dump */
  ws.buf = buf;
  ws.size = size;
  ws.pos = 0;
  status = luaU_dump(H, f, writer_2buf, &ws);
  return status;
}



/*
** {======================================================
** Parser functions
** =======================================================
*/

int hksI_parser_file(hksc_State *H, const char *filename) {
  int status;
  startcycle(H);
  status = loadfile(H, filename); /* parse file */
  endcycle(H);
  return status;
}


int hksI_parser_buf(hksc_State *H, const char *buf, size_t size,
                    const char *source) {
  int status;
  startcycle(H);
  status = loadbuffer(H, buf, size, source);
  endcycle(H);
  return status;
}



int hksI_parser_file2file(hksc_State *H, const char *filename,
                          const char *outname) {
  int status;
  Proto *f;
  startcycle(H);
  status = loadfile(H, filename); /* parse file */
  if (status) goto err;
  status = dump2file(H, filename, outname); /* dump to output file */
  err:
  endcycle(H);
  return status;
}


int hksI_parser_file2buf(hksc_State *H, const char *filename, char *out,
                         size_t outsize) {
  int status;
  Proto *f;
  lua_assert(out != NULL && outsize != 0);
  startcycle(H);
  status = loadfile(H, filename); /* parse file */
  if (status) goto err;
  status = dump2buf(H, out, outsize); /* dump to output file */
  err:
  endcycle(H);
  return status;
}


int hksI_parser_buf2file(hksc_State *H, const char *buf, size_t size,
                         const char *source, const char *outname) {
  int status;
  Proto *f;
  startcycle(H);
  status = loadbuffer(H, buf, size, source); /* parse buffer */
  if (status) goto err;
  status = dump2file(H, source, outname); /* dump to output buffer */
  err:
  endcycle(H);
  return status;
}


int hksI_parser_buf2buf(hksc_State *H, const char *buf, size_t size,
                        const char *source, char *out, size_t outsize) {
  int status;
  Proto *f;
  lua_assert(out != NULL && outsize != 0);
  startcycle(H);
  status = loadbuffer(H, buf, size, source); /* parse buffer */
  if (status) goto err;
  status = dump2buf(H, out, outsize); /* dump to output buffer */
  err:
  endcycle(H);
  return status;
}



/* }====================================================== */


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

