/*
** $Id: hksctest.c $
** Hksc tests
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define hksctest_c

#include "hksclib.h"

#ifdef HKSC_TESTING

#include "hkscaux.h"

extern const struct Opts opts;
extern const char *progname;

static Buffer testfile;  /* the current test source file name */
static Buffer dumpfile;
static Buffer expectfile;

static struct {
  int yes;
  const char *file1;
  const char *file2;
} showdiff;

#define error_check(a,b,c) 0

static int writer_2file(hksc_State *H, const void *p, size_t size, void *u) {
  size_t n;
  (void)H;
  n = fwrite(p,size,1,(FILE*)u);
  return (n!=1) && (size!=0);
}

#define dump2file(H,b) do { \
  f = xopen(buff_get(&b), "wb"); \
  status = lua_dump(H, writer_2file, f); \
  fclose(f); \
  if (status) return status; \
} while (0)

#define LUA_ERREXPECT 10001

#define COMPARE(b1,b2,text) do { \
  int result = fcmp(buff_get(&b1), buff_get(&b2), text); \
  if (result == 0) \
    printf("%s OK\n", buff_get(&b1)); \
  else { \
    showdiff.yes = 1; \
    showdiff.file1 = buff_get(&b1); \
    showdiff.file2 = buff_get(&b2); \
    return LUA_ERREXPECT; \
  } \
} while (0)


static int test_dump_f (hksc_State *H, void *ud) {
  FILE *f;
  int status;
  int callstackdb;  /* true if codt6/t7 */
  const char *filename = (const char *)ud;
  /* generate stripped bytecode */
  replace_ext(&expectfile, filename, ".cexpect");
  replace_ext(&dumpfile, filename, ".luac");
  lua_setstrip(H, BYTECODE_STRIPPING_ALL);
  dump2file(H, dumpfile);
  /* compare stripped bytecode to expected */
  COMPARE(dumpfile, expectfile, 0);
  /* generate profile info */
  replace_ext(&expectfile, filename, ".profileexpect");
  replace_ext(&dumpfile, filename, ".luaprofile");
#ifdef LUA_CODT6
  callstackdb = 1;
  lua_setstrip(H, BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION);
#else
  callstackdb = 0;
  lua_setstrip(H, BYTECODE_STRIPPING_PROFILING);
#endif
  dump2file(H, dumpfile);
  /* compare profile info with expected */
  COMPARE(dumpfile, expectfile, callstackdb);
  /* generate debug info */
  replace_ext(&expectfile, filename, ".debugexpect");
  replace_ext(&dumpfile, filename, ".luadebug");
#ifdef LUA_CODT6
  lua_setstrip(H, BYTECODE_STRIPPING_DEBUG_ONLY);
#else
  lua_setstrip(H, BYTECODE_STRIPPING_NONE);
#endif
  dump2file(H, dumpfile);
  /* compare debug info with expected */
  COMPARE(dumpfile, expectfile, 0);
  return status;
}

struct Redump {
  int strip;
  const char *filename;
};

static int test_redump_f (hksc_State *H, void *ud) {
  int status = 0;
  FILE *f;
  struct Redump *s = ud;
  const char *filename = s->filename;
  lua_setstrip(H, s->strip);
  switch (s->strip) {
    case BYTECODE_STRIPPING_ALL:
      replace_ext(&expectfile, filename, ".cexpect");
      replace_ext(&dumpfile, filename, ".credump");
      dump2file(H, dumpfile);
      COMPARE(dumpfile, expectfile, 0);
      break;
#ifdef LUA_CODT6
    case BYTECODE_STRIPPING_DEBUG_ONLY:
      lua_setstrip(H, BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION);
#endif
      /* fallthrough */
    case BYTECODE_STRIPPING_PROFILING:
      replace_ext(&expectfile, filename, ".profileexpect");
      replace_ext(&dumpfile, filename, ".profileredump");
      dump2file(H, dumpfile);
#ifndef LUA_CODT6
      COMPARE(dumpfile, expectfile, 0);
      break;
#else
      COMPARE(dumpfile, expectfile, 1);
#endif
      /* fallthrough */
    case BYTECODE_STRIPPING_NONE:
      lua_setstrip(H, s->strip);
      replace_ext(&expectfile, filename, ".debugexpect");
      replace_ext(&dumpfile, filename, ".debugredump");
      dump2file(H, dumpfile);
      COMPARE(dumpfile, expectfile, 0);
      break;
  }
  remove(buff_get(&dumpfile));
  return status;
}

static int test_files (hksc_State *H, int nfiles, char *files []) {
  int i, status;
#ifdef LUA_CODT6
  static Buffer debugfile;
#endif
  static Buffer redumpfile;
  struct Redump s;
  for (i = 0; i < nfiles; i++) {
    showdiff.yes = 0;
    buff_concat(&testfile, files[i]);
    if (1)
    status = hksI_parser_file(H, buff_get(&testfile), test_dump_f, files[i]);
    else
      status = 0, printf("testing '%s'\n", buff_get(&testfile));
    buff_revert(&testfile);
    if (status)
      goto fail;
    /* re-dump */
    replace_ext(&redumpfile, files[i], ".luac");
    s.strip = BYTECODE_STRIPPING_ALL;
    s.filename = buff_get(&redumpfile);
    status = hksI_parser_file(H, s.filename, test_redump_f, &s);
    if (status == 0) {
#ifdef LUA_CODT6
    replace_ext(&debugfile, files[i], ".luadebug");
    hksI_setdebugfile(H, buff_get(&debugfile));
    s.strip = BYTECODE_STRIPPING_DEBUG_ONLY;
    status = hksI_parser_file(H, s.filename, test_redump_f, &s);
#else
    replace_ext(&redumpfile, files[i], ".luaprofile");
    s.strip = BYTECODE_STRIPPING_PROFILING;
    s.filename = buff_get(&redumpfile);
    status = hksI_parser_file(H, s.filename, test_redump_f, &s);
    if (status == 0) {
      replace_ext(&redumpfile, files[i], ".luadebug");
      s.strip = BYTECODE_STRIPPING_NONE;
      s.filename = buff_get(&redumpfile);
      status = hksI_parser_file(H, s.filename, test_redump_f, &s);
    }
#endif
    }
    if (status) {
      fail:
      if (showdiff.yes) {
        fprintf(stderr, "FAILURE: %s: compilation differs from expected\n",
                files[i]);
        hksI_cmpfiles(H, showdiff.file1, showdiff.file2);
      }
      else
        fprintf(stderr, "%s: redump error: %s\n", files[i], lua_geterror(H));
      abort();
    }
  }
  return 0;
}

int test_main (hksc_State *H, int nfiles, char *files []) {
  if (opts.testsrcdir != NULL) {
    buff_concat(&testfile, opts.testsrcdir);
    /* ensure there is a directory separator after the source path */
    if (testfile.n && !IS_DIR_SEP(buff_get(&testfile)[buff_len(&testfile)-1]))
      buff_concat(&testfile, "/");
    /* add a file prefix map */
    buff_concat(&testfile, "=");
    lua_addprefixmap(H, buff_get(&testfile));
    buff_revert(&testfile);
  }
  buff_space(&testfile, 256);  /* space to concat each test file name */
  /* ensure debug info loader callbacks happen */
  lua_setignoredebug(H, 0);
  if (opts.expect_error)
    return error_check(H, nfiles, files);
  else
    return test_files(H, nfiles, files);
}

#endif /* HKSC_TESTING */
