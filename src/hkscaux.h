/*
** $Id: hkscdump.c $
** Dumping bytecode to files
** See Copyright Notice in lua.h
*/

#ifndef hkscaux_h
#define hkscaux_h

#include "lua.h"

#include "lzio.h"

#ifdef LUA_COD
# ifdef HKSC_DECOMPILER

extern int init_debug_reader(hksc_State *H, ZIO *z, Mbuffer *buff,
                             char *udata_buff, const char *name);
extern int close_debug_reader(hksc_State *H, ZIO *z, Mbuffer *buff,
                              char *udata_buff, const char *name);

# endif /* HKSC_DECOMPILER */

extern void luacod_startcycle(hksc_State *H, const char *name);
extern void luacod_endcycle(hksc_State *H, const char *name);

#endif /* LUA_COD */


extern int hksc_dump_function(hksc_State *H, const Proto *f,
                              const char *filename);

#endif /* hkscaux_h */
