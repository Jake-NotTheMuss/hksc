/*
** $Id: hksctest.c $
** Hksc tests
** See Copyright Notice in lua.h
*/

#include <assert.h>
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
  buff_concat(buff, "diff -u --strip-trailing-cr ");
  buff_concat(buff, a);
  buff_concat(buff, " ");
  buff_concat(buff, b);
  return buff_get(buff);
}

#define BUILD_DIFF_CMD(d,a,b)  build_diff_cmd(&d, buff_get(&a), buff_get(&b))

static struct {
  const char *a;
  const char *b;
} test_file_comparison;


static int writer_2file(hksc_State *H, const void *p, size_t size, void *u) {
  size_t n;
  (void)H;
  n = fwrite(p,size,1,(FILE*)u);
  return (n!=1) && (size!=0);
}

#ifdef HKSC_DECOMP_DEBUG_PASS1
#define DECOMP_EXT ".decp1.lua"
#else
#define DECOMP_EXT ".dec.lua"
#endif

static char *transform_name (const char *name) {
  static Buffer transform_buffer;
  static const char decomp_ext [] = DECOMP_EXT;
  char *s;
  buff_reset(&transform_buffer);
  buff_concat(&transform_buffer, name);
  s = strstr(buff_get(&transform_buffer), decomp_ext);
  if (s != NULL) {
    /* omit `.dec' */
    const char *ext = s + (strstr(decomp_ext, ".lua") - decomp_ext);
    memmove(s, ext, strlen(ext));
    s[strlen(ext)] = 0;
  }
  return buff_get(&transform_buffer);
}

#define dump2file(H,b) do { \
  FILE *f = xopen(buff_get(&b), "wb"); \
  status = lua_dump(H, writer_2file, f); \
  fclose(f); \
  if (status) return status; \
} while (0)

#define LUA_ERREXPECT 10001

#define COMPARE(b1,b2,text,printok,action) do { \
  int result; \
  result = fcmp(buff_get(&b1), buff_get(&b2), text); \
  if (result == 0) { \
    if (printok) printf("%s %sOK\n", transform_name(buff_get(&b1)), \
      sizeof(action) > 1 ? action " " : ""); \
  } \
  else { \
    test_file_comparison.a = buff_get(&b1); \
    test_file_comparison.b = buff_get(&b2); \
    return LUA_ERREXPECT; \
  } \
} while (0)

static int decompiling;

static int test_dump_f (hksc_State *H, void *ud) {
  int status;
  int callstackdb;  /* true if codt6/t7 */
  const char *filename = (const char *)ud;
  const char *testfilename = basename(buff_get(&testfile));
  /* generate stripped bytecode */
  replace_ext(&expectfile, testfilename,
              opts.test_struct ? ".cexpect1" : ".cexpect");
  replace_ext(&dumpfile, filename, ".luac");
  lua_setstrip(H, BYTECODE_STRIPPING_ALL);
  dump2file(H, dumpfile);
  /* compare stripped bytecode to expected */
  COMPARE(dumpfile, expectfile, 0, 1, "");
  if (opts.test_struct || (decompiling && opts.ignore_debug))
    return status;
  /* generate profile info */
  replace_ext(&expectfile, testfilename, ".profileexpect");
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
  replace_ext(&expectfile, testfilename, ".debugexpect");
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

#ifdef HKSC_DECOMPILER
static int dump_decomp (hksc_State *H, void *ud) {
  int status;
  FILE *f;
  const char *filename = (const char *)ud;
  replace_ext(&dumpfile, filename, DECOMP_EXT);
  f = xopen(buff_get(&dumpfile), "w");
  status = lua_decompile(H, writer_2file, f);
  fclose(f);
  if (status)
    return status;
#ifdef HKSC_DECOMP_DEBUG_PASS1
  if (opts.ignore_debug) {
    replace_ext(&expectfile, filename, ".p1nexpect");
    /* if p1nexpect does not exist, use p1expect */
    f = fopen(buff_get(&expectfile), "r");
    if (f != NULL) {
      fclose(f);
      goto testp1;
    }
  }
  replace_ext(&expectfile, filename, ".p1expect");
  testp1:
  COMPARE(dumpfile, expectfile, 1, 1, "decompiled");
#endif
  return status;
}

static void test_decomp (hksc_State *H, char *file) {
  static Buffer decompfile;
  static Buffer prefixmap;
  int status;
  FILE *log;
  decompiling = 1;
  buff_reset(&decompfile);
  buff_concatn(&decompfile, basename(file), strstr(file, ".lua") - file);
  buff_concat(&decompfile, ".dec.log");
  log = xopen(buff_get(&decompfile), "w");
  lua_setlogfile(H, log);
  printf("Decompiling %s\n", file);
  status = hksI_parser_file(H, buff_get(&testfile), dump_decomp, file);
  fclose(log);
  log = NULL;
  if (status) {
    error:
    if (status == LUA_ERREXPECT)
      system(BUILD_DIFF_CMD(command, dumpfile, expectfile));
    else
      fprintf(stderr, "%s: %s\n", file, lua_geterror(H));
    abort();
  }
  buff_reset(&prefixmap);
#ifndef HKSC_DECOMP_DEBUG_PASS1
  buff_concat(&prefixmap, buff_get(&dumpfile));
  buff_concat(&prefixmap, "=");
  buff_concat(&prefixmap, file);
  lua_addprefixmap(H, buff_get(&prefixmap));
  /* move DUMPFILE to another buffer that will not be used in the dump
     function */
  buff_reset(&decompfile);
  buff_concat(&decompfile, buff_get(&dumpfile));
  /* compile the decompiled program and compare with expected */
  status = hksI_parser_file(H, buff_get(&dumpfile), test_dump_f,
                            buff_get(&decompfile));
  lua_removeprefixmap(H, buff_get(&prefixmap));
  if (status) {
    if (status == LUA_ERREXPECT) {
      int code;
      static Buffer diff_a, diff_b;
      /* diff <source file> <decompiled output> */
      buff_reset(&diff_a);
      buff_concat(&diff_a, file);
      buff_move(&diff_b, &decompfile);
      fprintf(stderr, "%s: decompilation differs from original\n", file);
      fprintf(stderr, "\ntext comparison:\n\n");
      code = system(BUILD_DIFF_CMD(command, diff_a, diff_b));
      if (code == 0)
        printf("<no differences found>\n");
      fprintf(stderr, "\n");
      assert(test_file_comparison.a != NULL);
      assert(test_file_comparison.b != NULL);
      fprintf(stderr, "binary comparison:\n\n");
      hksI_cmpfiles(H, test_file_comparison.a, test_file_comparison.b);
      abort();
    }
    goto error;
  }
#endif /* !HKSC_DECOMP_DEBUG_PASS1 */
  decompiling = 0;
}
#endif /* HKSC_DECOMPILER */

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

#define END_TEST() buff_revert(&testfile)

static int test_files (hksc_State *H, int nfiles, char *files []) {
  int i, status;
#ifdef LUA_CODT6
  static Buffer debugfile;
#endif
  static Buffer redumpfile;
  struct Redump s;
  for (i = 0; i < nfiles; i++) {
    test_file_comparison.a = test_file_comparison.b = NULL;
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
    if (status)
      goto fail;
#ifdef HKSC_DECOMPILER
    test_decomp(H, files[i]);
#endif /* HKSC_DECOMPILER */
    }
    if (status) {
      fail:
      if (status == LUA_ERREXPECT) {
        assert(test_file_comparison.a != NULL);
        assert(test_file_comparison.b != NULL);
        fprintf(stderr, "FAILURE: %s: compilation differs from expected\n",
                files[i]);
        hksI_cmpfiles(H, test_file_comparison.a, test_file_comparison.b);
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
