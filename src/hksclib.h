/*
** hksclib.h
** A wrapper for the modded Lua library
** See Copyright Notice in lua.h
*/


#ifndef HKSC_LIB_H
#define HKSC_LIB_H

#include "lua.h"


typedef int (*hksc_DumpFunction) (hksc_State *H, const Proto *f, void *ud);

/*
** API declarations
*/

/* Lua state constructor/destructor */
LUA_API hksc_State *hksI_newstate(int mode);
LUA_API void hksI_close(hksc_State *H);


/* Custom parse & dump functions (user program must provide a custom dump
   function to call when dumping the bytecode) */
LUA_API int hksI_parser_file(hksc_State *H, const char *filename,
                             hksc_DumpFunction dumpf, void *ud);
LUA_API int hksI_parser_buffer(hksc_State *H, const char *buff, size_t size,
                               char *source, hksc_DumpFunction dumpf, void *ud);


/* extra error code for `luaL_load' */
#define LUA_ERRFILE     (LUA_ERRERR+1)
/*#define LUA_ERRMODE     (LUA_ERRERR+2)
#define LUA_ERRBYTECODE (LUA_ERRERR+3)*/

#endif /* HKSC_LIB_H */
