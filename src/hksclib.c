#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>

#include "hksc_begin_code.h"

#include "lua.h"
#include "lzio.h"
#include "lstate.h"
#include "lerror.h"

#include "hksclib.h"

#include "lparser.h"

#include "lundump.h"

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


static const char *getF(hksc_State *H, void *ud, size_t *size)
{
  UNUSED(H);
  LoadF *lf = (LoadF *)ud;

  if (lf->extraline)
  {
    lf->extraline = 0;
    *size = 1;
    return "\n";
  }
  if (feof(lf->f)) return NULL;
  *size = fread(lf->buff, 1, sizeof(lf->buff), lf->f);
  return (*size > 0) ? lf->buff : NULL;
}

static int errfile (hksc_State *H, const char *what, const char *filename) {
  const char *serr = strerror(errno);
  hksc_setfmsg(H, "cannot %s %s: %s", what, filename, serr);
  return LUA_ERRFILE;
}

static int parser(hksc_State *H, ZIO *z, const char *filename);

int
hksc_parsefile(hksc_State *H, const char *filename)
{
  LoadF lf;
  int status, readstatus;
  int c;
  lf.extraline = 0;

  if (filename == NULL)
    lf.f = stdin;
  else
  {
    lf.f = fopen(filename, "r");
    if (lf.f == NULL) return errfile(H, "open", filename);
  }

  c = getc(lf.f);
  if (c == '#') /* Unix exec. file? */
  {
    lf.extraline = 1;
    while ((c = getc(lf.f)) != EOF && c != '\n') ; /* skip first line */
    if (c == '\n') c = getc(lf.f);
  }

  if (c == LUA_SIGNATURE[0] && lf.f != stdin) /* binary file? */
  {
    fclose(lf.f);
    fprintf(stderr,
      "%s is already a pre-compiled Lua file, skipping\n",
      filename);
    return 1;
  }

  ungetc(c, lf.f);

  /* lua_load */
  /*status = hksc_load(getF, &lf, filename);*/
  ZIO z;
  luaZ_init(H, &z, getF, &lf);
  status = parser(H, &z, filename);


  return status;
}

static int
writer(hksc_State *H, const void *p, size_t sz, void *ud)
{
  UNUSED(H);
  FILE *out = (FILE *)ud;
  if (fwrite(p, 1, sz, out) != sz)
    return 1;
  else
    return 0;
}

static int
parser(hksc_State *H, ZIO *z, const char *filename)
{
  int status;
  char *outname;
  FILE *out;

  if (filename != NULL)
  {
    size_t len = strlen(filename);
    /* +5 for ".luac" */
    outname = malloc(len + sizeof(LUAC_EXT));
    if (!outname)
    {
      fprintf(stderr, "Out of memory\n");
      exit(EXIT_FAILURE);
    }

    strncpy(outname, filename, len);

    char *s = strrchr(filename, '.');
    if (s && strcmp(s, ".lua") == 0)
    {
      size_t x = (size_t)(s - filename);
      strncpy(outname + x, LUAC_EXT, sizeof(LUAC_EXT));
    }
    else
      strncpy(outname + len, LUAC_EXT, sizeof(LUAC_EXT));

    out = fopen(outname, "wb");
    if (out == NULL) return errfile(H, "open", outname);
  }
  else
  {
    fprintf(stderr, "Need a filename\n");
    return 1;
  }
  printf("outname: %s\n", outname);

  /* used to determine of compiling or decompiling */
  /*int c = luaZ_lookahead(z);*/

  Proto *f;
  status = luaD_protectedparser(H, z, filename, &f);
  if (status) return status;

  /*printf("f: %p\n", f);*/
  status = luaU_dump(H, f, writer, out, 1);
  fclose(out);
  free(outname);
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

hksc_State *
hksc_xnewstate(void)
{
  hksc_State *H = hksc_newstate(l_alloc, NULL);
  if (H) hksc_atpanic(H, &panic);
  else
  {
    fprintf(stderr, "cannot allocate a hksc_State\n");
    exit(EXIT_FAILURE);
  }

  return H;
}
