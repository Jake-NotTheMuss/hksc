/*
** $Id: hkscaux.c $
** Dumping bytecode to files
** See Copyright Notice in lua.h
*/

#ifndef hkscaux_h
#define hkscaux_h

#include "hksclua.h"

#ifdef LUA_COD

extern void luacod_startcycle(hksc_State *H, const char *name);
extern void luacod_endcycle(hksc_State *H, const char *name);

#endif /* LUA_COD */

/* dumps or decompiles bytecode */
extern int hksc_dump_bytecode(hksc_State *H, const char *filename);
#ifdef HKSC_DECOMPILER
extern int hksc_dump_decomp(hksc_State *H, const char *filename);
#endif /* HKSC_DECOMPILER */

#endif /* hkscaux_h */
