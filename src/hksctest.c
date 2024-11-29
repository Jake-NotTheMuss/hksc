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
static Buffer command;  /* argument to `system()' */

static char *build_diff_cmd (Buffer *buff, const char *a, const char *b) {
  buff_concat(buff, "diff -u ");
  buff_concat(buff, a);
  buff_concat(buff, " ");
  buff_concat(buff, b);
  return buff_get(buff);
}

#define BUILD_DIFF_CMD(d,a,b)  build_diff_cmd(&d, buff_get(&a), buff_get(&b))

static struct {
  int yes;
  const char *file1;
  const char *file2;
} showdiff;


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

#define COMPARE(b1,b2,text,printok,action) do { \
  int result = fcmp(buff_get(&b1), buff_get(&b2), text); \
  if (result == 0) { \
    if (printok) printf("%s %sOK\n", buff_get(&b1), \
         sizeof(action) > 1 ? action " " : ""); \
  } \
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
  replace_ext(&expectfile, filename,
              opts.test_struct ? ".cexpect1" : ".cexpect");
  replace_ext(&dumpfile, filename, ".luac");
  lua_setstrip(H, BYTECODE_STRIPPING_ALL);
  dump2file(H, dumpfile);
  /* compare stripped bytecode to expected */
  COMPARE(dumpfile, expectfile, 0, 1, "");
  if (opts.test_struct)
    return status;
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
  COMPARE(dumpfile, expectfile, callstackdb, 1, "");
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
  COMPARE(dumpfile, expectfile, 0, 1, "");
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
      replace_ext(&dumpfile, filename,
                  /* for struct tests, this name will be printed when the test
                     finishes, so I would rather it be shown as .luac,
                     but non-struct tests will still use .luac afterward, so
                     use a different name */
                  opts.test_struct ? ".luac" : ".credump");
      dump2file(H, dumpfile);
      COMPARE(dumpfile, expectfile, 0, opts.test_struct, "redumped");
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
      COMPARE(dumpfile, expectfile, 0, 0, "redumped");
      break;
#else
      COMPARE(dumpfile, expectfile, 1, 0, "redumped");
#endif
      /* fallthrough */
    case BYTECODE_STRIPPING_NONE:
      lua_setstrip(H, s->strip);
      replace_ext(&expectfile, filename, ".debugexpect");
      replace_ext(&dumpfile, filename, ".debugredump");
      dump2file(H, dumpfile);
      COMPARE(dumpfile, expectfile, 0, 0, "redumped");
      break;
  }
  remove(buff_get(&dumpfile));
  return status;
}

#define BEGIN_TEST(file) do { \
  const char *dot; \
  int n; \
  buff_concat(&testfile, file); \
  if (opts.expect_error) break; \
  printf("------------ "); \
  dot = strchr(file, '.'); \
  if (dot == NULL) \
    n = (int)strlen(file); \
  else \
    n = dot - file; \
  printf("%.*s", n, file); \
  printf(" ------------\n"); \
} while (0)

#define END_TEST() buff_revert(&testfile);

static int test_files (hksc_State *H, int nfiles, char *files []) {
  int i, status;
#ifdef LUA_CODT6
  static Buffer debugfile;
#endif
  static Buffer redumpfile;
  struct Redump s;
  for (i = 0; i < nfiles; i++) {
    showdiff.yes = 0;
    BEGIN_TEST(files[i]);
    status = hksI_parser_file(H, buff_get(&testfile), test_dump_f, files[i]);
    if (status)
      goto fail;
    /* re-dump */
    replace_ext(&redumpfile, files[i], ".luac");
    s.strip = BYTECODE_STRIPPING_ALL;
    s.filename = buff_get(&redumpfile);
    status = hksI_parser_file(H, s.filename, test_redump_f, &s);
    if (status == 0 && !opts.test_struct) {
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
    END_TEST();
  }
  return 0;
}

static int error_dump_f (hksc_State *H, void *ud) {
  (void)H; (void)ud;
  return 0;
}

static int error_check (hksc_State *H, int nfiles, char *files []) {
  int i, status;
  for (i = 0; i < nfiles; i++) {
    const char *msg;
    BEGIN_TEST(files[i]);
    status = hksI_parser_file(H, buff_get(&testfile), error_dump_f, NULL);
    if (status == 0) {
      fprintf(stderr,
              "%s: no error was raised\n", files[i]);
      abort();
    }
    msg = lua_geterror(H);
    if (msg == NULL) msg = "";
    replace_ext(&dumpfile, files[i], ".test");
    {
      FILE *f = xopen(buff_get(&dumpfile), "w");
      fprintf(f, "%s\n", msg);
      fclose(f);
    }
    replace_ext(&expectfile, files[i], ".expect");
    if (fcmp(buff_get(&dumpfile), buff_get(&expectfile), 0) == 0)
      printf("%s error check OK\n", files[i]);
    else {
      fprintf(stderr,
              "%s: error check failed (output differs from expected)\n\n",
              files[i]);
      system(BUILD_DIFF_CMD(command, dumpfile, expectfile));
      abort();
    }
    END_TEST();
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
