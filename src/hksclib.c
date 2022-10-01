#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>

#include "hksc_begin_code.h"

#include "lua.h"
#include "lzio.h"

#include "hksclib.h"

#include "./lparser.h"

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


static const char *getF(void *ud, size_t *size)
{
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


static int errfile (const char *what, const char *filename) {
  const char *serr = strerror(errno);
  fprintf(stderr, "cannot %s %s: %s", what, filename, serr);
  return LUA_ERRFILE;
}

static int parser(ZIO *z);

int
hksc_parsefile(const char *filename)
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
    if (lf.f == NULL) return errfile("open", filename);
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
  luaZ_init(&z, getF, &lf);
  status = parser(&z);


  return 0;
}

static int
parser(ZIO *z)
{
  Mbuffer buff;
  luaZ_initbuffer(&buff);

  /* used to determine of compiling or decompiling */
  /*int c = luaZ_lookahead(z);*/

  Proto *tf = hksc_parser(z, &buff);
  return 0;
}

int
hksc_init(void)
{
  return hksc_parser_init();
}
