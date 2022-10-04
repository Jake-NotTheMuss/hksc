/* All logic to be processed at the start of an hksc translation unit.
   Makes sure hksc interacts smoothly with modded Lua.

   IMPORTANT: This file must be included in any hksc file BEFORE any non-system
   headers are included.  */

#ifndef HKSC_BEGIN_CODE_H
#define HKSC_BEGIN_CODE_H

#include <stddef.h>

#define LUAC_EXT ".luac"

/* same as lua_Reader but without lua_State */
typedef const char * (*hksc_Reader) (void *ud, size_t *sz);
typedef int (*hksc_Writer) (const void *p, size_t sz, void *ud);

#ifndef LUA_CORE
#define LUA_CORE
#endif

#include "luaconf.h"
#include "lua.h" /* typedef lua_Reader before it gets redefined */
#define lua_Reader hksc_Reader
#define lua_Writer hksc_Writer

#undef LUA_CORE

/* need to include all hksc-versions of Lua headers first, so that the
   Lua versions never get processed if accidentally included */
/* Be mindful of inter-dependencies with headers */
#include "./lmem.h" /* stateless memory module */
#include "./lzio.h" /* stateless stream-io module */
#include "./lobject.h"
#include "./lparser.h"
#include "./lfunc.h"
#include "./ltable.h"
#include "./lstring.h"
#include "./lopcodes.h"
#include "./llex.h"
#include "./lcode.h"


/* extra error code for `luaL_load' */
#ifndef LUA_ERRFILE
#define LUA_ERRFILE     (LUA_ERRERR+1)
#endif /* LUA_ERRFILE */

#endif /* HKSC_BEGIN_CODE_H */
