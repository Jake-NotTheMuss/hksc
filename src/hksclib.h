/*
** hksclib.h
** A wrapper for the modded Lua library
** See Copyright Notice in lua.h
*/


#ifndef HKSC_LIB_H
#define HKSC_LIB_H

#include "luaconf.h"
#include "lua.h"

#include "lstate.h"
#include "lobject.h" /* Proto */


typedef int (*hksc_DumpFunction) (hksc_State *H, Proto *f, void *ud);

/*
** `dump context' for dumping bytecode to a memory buffer
*/
typedef struct hksc_DumpCtx {
  hksc_State *H;
  lua_Alloc frealloc;  /* function to reallocate memory */
  void *ud;         /* auxiliary data to `frealloc' */
  char *buff;
  size_t alloc; /* total size allocated */
  size_t size; /* size of dumped data (<= alloc) */
} hksc_DumpCtx;


/*
** API declarations
*/

/* Lua state constructor/destructor */
LUA_API hksc_State *hksI_newstate(int mode);
LUA_API void hksI_close(hksc_State *H);


/* Dump context constructor/destructor */
LUA_API void hksI_dumpctx_init(hksc_State *H, hksc_DumpCtx *ctx);
LUA_API void hksI_dumpctx_free(hksc_State *H, hksc_DumpCtx *ctx);


/* Syntax check functions (does not dump bytecode) */
LUA_API int hksI_parser_file(hksc_State *H, const char *filename);
LUA_API int hksI_parser_buff(hksc_State *H, const char *buff, size_t size,
                               const char *source);


/* Standard parse & dump functions (file -> file / buffer -> file) */
/* file --> file */
LUA_API int hksI_parser_file2file(hksc_State *H, const char *filename,
                                    const char *outname);
/* file --> buffer */
LUA_API int hksI_parser_buff2file(hksc_State *H, const char *buff,
                        size_t size, const char *source, const char *outname);
/* file --> buffer */
LUA_API int hksI_parser_file2buff(hksc_State *H, const char *filename,
                                    hksc_DumpCtx *ctx);
/* buffer --> buffer */
LUA_API int hksI_parser_buff2buff(hksc_State *H, const char *buff,
                      size_t size, const char *source, hksc_DumpCtx *ctx);

/* Notes:
   (*) Functions which output to files do not require an output file name to be
       specified. If the output name is NULL, one will be generated from the
       input name.

   (*) Functions which output to buffers require the construction beforehand and
       the destruction afterward of an hksc_DumpCtx structure. This structure
       will hold the pointer to the dumped bytecode buffer, and the user program
       is responsible for freeing that memory using `hksI_dumpctx_free()'.
*/


/* Custom parse & dump functions (user program must provide a custom dump
   function to call when dumping the bytecode) */
LUA_API int hksI_parser_file_dump(hksc_State *H, const char *filename,
                                    hksc_DumpFunction dumpf, void *ud);
LUA_API int hksI_parser_buff_dump(hksc_State *H, const char *buff,
          size_t size, const char *source, hksc_DumpFunction dumpf, void *ud);


/* extra error code for `luaL_load' */
#define LUA_ERRFILE     (LUA_ERRERR+1)
/*#define LUA_ERRMODE     (LUA_ERRERR+2)
#define LUA_ERRBYTECODE (LUA_ERRERR+3)*/

#endif /* HKSC_LIB_H */
