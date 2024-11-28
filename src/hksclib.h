/*
** hksclib.h
** A wrapper for the modded Lua library
** See Copyright Notice in lua.h
*/


#ifndef HKSC_LIB_H
#define HKSC_LIB_H

#include "hksclua.h"


typedef int (*hksc_Dumper) (hksc_State *H, void *ud);

/*
** Auxiliary API
*/

/* Lua state constructor/destructor */
LUA_API void hksI_compilersettings(hksc_CompilerSettings *settings);
LUA_API void hksI_settings(hksc_StateSettings *settings);
LUA_API hksc_State *hksI_newstate(hksc_StateSettings *settings);
LUA_API void hksI_close(hksc_State *H);

/*
** Functions for performing translation cycles
** All pointer arguments must be non-NULL
*/

/*
** hksI_parser - Run a translation cycle with a given reader function.
**  param H is the Lua state
**  param rf is the reader function
**  param rd is the userdata for the reader function
**  param df is the dump function
**  param dd is the userdata for the dump function
**  param chunkname is the name of the compiled chunk
**  All arguments must be non-NULL
*/
LUA_API int hksI_parser(hksc_State *H, lua_Reader rf, void *rd,
                        hksc_Dumper df, void *dd, const char *chunkname);

/*
** hksI_parser_file - Run a translation cycle on the contents of a file.
**  param H is the Lua state
**  param filename is the name of the file to parse
**  param df is the dump function
**  param ud is the userdata for the dump function
** The source name will be generated based on the filename.
*/
LUA_API int hksI_parser_file(hksc_State *H, const char *filename,
                             hksc_Dumper df, void *ud);

/*
** hksI_parser_buffer - Run a translation cycle on a string.
**  param H is the Lua state
**  param buff is the pointer to the string
**  param size is the length of the string
**  param source is the name of the string, used to generate the chunk name
**  param df is the dump function
**  param ud the userdata for the dump function
*/
LUA_API int hksI_parser_buffer(hksc_State *H, const char *buff, size_t size,
                               const char *source, hksc_Dumper df, void *ud);


/*
** These functions apply only to CODT6 mode, where debug info is in a separate
** location from the bytecode
*/
#ifdef LUA_CODT6
/*
** Functions for controlling how debug info is loaded. All `name' parameters
** are used as a name for the debug info source to use in error messages by the
** library, and may be NULL
*/

/* Use a given reader function to load debug info.  */
LUA_API void hksI_setdebugreader (hksc_State *H, lua_Reader r, void *ud,
                                  const char *name);

/* Load debug info from the given file.  */
LUA_API void hksI_setdebugfile (hksc_State *H, const char *filename);

/* Load debug info from the given string.  */
LUA_API void hksI_setdebugbuffer (hksc_State *H, const void *buff,
                                  size_t size, const char *name);
#endif /* LUA_CODT6 */

#ifdef HKSC_TESTING
LUA_API int hksI_cmpfiles (hksc_State *H,
                           const char *file1, const char *file2);
#endif

/* extra error code for `luaL_load' */
#define LUA_ERRFILE     (LUA_ERRERR+1)

#endif /* HKSC_LIB_H */
