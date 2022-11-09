/*
** hksclib.h
** A wrapper for the modded Lua library
** See Copyright Notice in lua.h
*/


#ifndef HKSC_LIB_H
#define HKSC_LIB_H

#include "lstate.h"


/*
** API declarations
*/

/* create a new Lua state */
LUAI_FUNC hksc_State *hksI_newstate(int mode);
/* close a Lua state */
LUAI_FUNC void hksI_close(hksc_State *H);

/* parse a file, do not dump */
LUAI_FUNC int hksI_parser_file(hksc_State *H, const char *filename);
/* parse a buffer, do not dump */
LUAI_FUNC int hksI_parser_buf(hksc_State *H, const char *buf, size_t size,
                              const char *source);
/* parse a file and dump to a file */
LUAI_FUNC int hksI_parser_file2file(hksc_State *H, const char *filename,
                                   const char *outname);
/* parse a file and dump to a buffer */
LUAI_FUNC int hksI_parser_file2buf(hksc_State *H, const char *filename,
                                  char *out, size_t outsize);
/* parse a buffer and dump to a file */
LUAI_FUNC int hksI_parser_buf2file(hksc_State *H, const char *buf, size_t size,
                                  const char *source, const char *outname);
/* parse a buffer and dump to a buffer */
LUAI_FUNC int hksI_parser_buf2buf(hksc_State *H, const char *buf, size_t size,
                              const char *source, char *out, size_t outsize);

/* extra error code for `luaL_load' */
#define LUA_ERRFILE     (LUA_ERRERR+1)

#endif /* HKSC_LIB_H */
