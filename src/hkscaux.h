/*
** $Id: hkscaux.c $
** Dumping bytecode to files
** See Copyright Notice in lua.h
*/

#ifndef hkscaux_h
#define hkscaux_h

#include "hksclua.h"

#define MAX_PREFIX_MAPS 32

#define STDIN_NAME "stdin" /* stdin output file name */

struct Opts {
  int listing;
  int dumping;
  int compile;
  int decompile;
  int mode;
  int strip;
  int literals;
  int ignore_debug;
  int from_stdin;  /* stdin is an infile */
  const char *output;
  int with_debug;
#ifdef HKSC_TESTING
  int testing;
  int expect_error;
#endif
#ifdef HKSC_MULTIPLAT
  int plat;
  int wordsize;
#endif
#ifdef LUA_CODT6
  const char *debug_file;
  const char *callstack_file;
#endif
  int nprefixmaps;
  const char *prefixmaps [MAX_PREFIX_MAPS];
};

#ifdef LUA_CODT6

extern void luacod_startcycle(hksc_State *H, const char *name);
extern void luacod_endcycle(hksc_State *H, const char *name);

#endif /* LUA_CODT6 */

/* dumps or decompiles bytecode */
extern int hksc_list_bytecode(hksc_State *H, const char *filename, int full);
extern int hksc_dump_bytecode(hksc_State *H, const char *filename);
#ifdef HKSC_DECOMPILER
extern int hksc_dump_decomp(hksc_State *H, const char *filename);
#endif /* HKSC_DECOMPILER */

#endif /* hkscaux_h */
