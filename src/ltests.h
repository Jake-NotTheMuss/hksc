/*
** $Id: ltests.h $
** Internal Header for Debugging of the Lua Implementation
** See Copyright Notice in lua.h
*/

#ifndef ltests_h
#define ltests_h


#include <stdlib.h>


#define LUA_DEBUG

#undef NDEBUG
#include <assert.h>
#define lua_assert(c)           assert(c)


/* to avoid warnings, and to make sure value is really unused */
#define UNUSED(x)       (x=0, (void)(x))


/* set which decompiler pass to test */
#if HKSC_DEBUG_PASS == 1
# define HKSC_DECOMP_DEBUG_PASS1
# undef HKSC_DECOMP_DEBUG_PASS2
#elif HKSC_DEBUG_PASS == 2
# define HKSC_DECOMP_DEBUG_PASS2
# undef HKSC_DECOMP_DEBUG_PASS1
#else
# undef HKSC_DECOMP_DEBUG_PASS1
# undef HKSC_DECOMP_DEBUG_PASS2
#endif /* HKSC_DEBUG_PASS */


/* memory allocator control variables */
typedef struct Memcontrol {
  unsigned long numblocks;
  unsigned long total;
  unsigned long maxmem;
  unsigned long memlimit;
} Memcontrol;

LUAI_DATA Memcontrol memcontrol;


/*
** generic variable for debug tricks
*/
LUAI_DATA int Trick;


void *debug_realloc (void *ud, void *block, size_t osize, size_t nsize);

LUA_API hksc_State *debug_newstate (hksc_StateSettings *settings);

#ifdef hksc_c
# define hksI_newstate(settings) debug_newstate(settings)
#endif




/* test for lock/unlock */
#undef luai_userstateopen
#undef luai_userstatethread
#undef lua_lock
#undef lua_unlock
#undef LUAI_EXTRASPACE

struct L_EXTRA { int lock; int *plock; };
#define LUAI_EXTRASPACE   sizeof(struct L_EXTRA)
#define getlock(l)  (cast(struct L_EXTRA *, l) - 1)
#define luai_userstateopen(l)  \
  (getlock(l)->lock = 0, getlock(l)->plock = &(getlock(l)->lock))
#define luai_userstatethread(l,l1)  (getlock(l1)->plock = getlock(l)->plock)
#define lua_lock(l)     lua_assert((*getlock(l)->plock)++ == 0)
#define lua_unlock(l)   lua_assert(--(*getlock(l)->plock) == 0)


LUAI_FUNC void luaB_opentests (hksc_State *H);




/* change some sizes to give some bugs a chance */

#undef LUAL_BUFFERSIZE
#define LUAL_BUFFERSIZE   27
#define MINSTRTABSIZE   2

#endif
