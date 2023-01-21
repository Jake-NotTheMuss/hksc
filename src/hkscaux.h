/*
** $Id: hkscaux.c $
** Dumping bytecode to files
** See Copyright Notice in lua.h
*/

#ifndef hkscaux_h
#define hkscaux_h

#include "lua.h"

#ifdef LUA_COD

extern void luacod_startcycle(hksc_State *H, const char *name);
extern void luacod_endcycle(hksc_State *H, const char *name);

#endif /* LUA_COD */


extern int hksc_dump_function(hksc_State *H, const Proto *f,
                              const char *filename);

#endif /* hkscaux_h */
