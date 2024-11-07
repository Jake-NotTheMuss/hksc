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

int test_main (hksc_State *H, int nfiles, char *files []) {
  (void)H; (void)nfiles; (void)files;
  return 0;
}

#endif /* HKSC_TESTING */
