/*
** $Id: lgc.h $
** Garbage Collector
** See Copyright Notice in lua.h
*/

/******************************************************************************
  The garbage collector has 2 modes; it either happens in the middle of a
  translation cycle or in between translation cycles. In the former, it is a
  simpler version of the Lua 5.0 collector. It marks all objects reachable from
  the root state and global state, then sweeps. In the latter, it does not
  mark, only sweeps, and it does not collect objects marked with GC_TEMP,
  which will typically be objects used for internal representation, i.e.
  objects that only exist within a translation cycle, such as Proto or Table.
  This is a simpler alternative to having a Lua stack to temporarily anchor
  objects. GC_TEMP objects are still collected in between cycles if they are
  unreachable, as

  Non-fixed strings could also be collected on each new cycle, but instead they
  are only unmarked, and then maybe collected if the total memory usage is
  above the given threshold. The reasoning is that multiple Lua files may use
  the same strings (e.g. in using the same global variables), and so not
  collecting them every cycle avoids having to continuously deallocate and
  reallocate memory for the same string.
******************************************************************************/

#ifndef lgc_h
#define lgc_h


#include "lobject.h"


/*
** some useful bit tricks
*/
#define resetbits(x,m)  ((x) &= cast(lu_byte, ~(m)))
#define setbits(x,m)  ((x) |= (m))
#define testbits(x,m) ((x) & (m))
#define bitmask(b)  (1<<(b))
#define bit2mask(b1,b2) (bitmask(b1) | bitmask(b2))
#define l_setbit(x,b) setbits(x, bitmask(b))
#define resetbit(x,b) resetbits(x, bitmask(b))
#define testbit(x,b)  testbits(x, bitmask(b))
#define set2bits(x,b1,b2) setbits(x, (bit2mask(b1, b2)))
#define reset2bits(x,b1,b2) resetbits(x, (bit2mask(b1, b2)))
#define test2bits(x,b1,b2)  testbits(x, (bit2mask(b1, b2)))



/*
** Layout for bit use in `marked' field:
*/
enum {
  /* GC_TEMP is used to mark objects that should be considered reachable,
     because they are being used for internal representation, even though they
     are not reachable from any of the root structures; providing a limit of 0
     will keep objects marked GC_TEMP from being freed. If memory is only used
     for part of a compilation process, e.g. memory for parsing a single
     function, which can be freed leaving the function, then you can call
     `killtemp' on it, which will allow it to be freed if GC is triggered later
     in the same translation cycle. */
  GC_TEMP,
  GC_REACHABLE,  /* object is reachable */
  GC_FIXED  /* object should not be collected */
};

enum {
  /* collection kind from least objects collected to most objects collected */
  GC_COLLECT_KEEP_TEMP,  /* do not collect temporary objects */
  GC_COLLECT_NORMAL,  /* collect all unreachable objects */
  GC_COLLECT_ALL  /* collect all objects */
};


#define luaC_checkGC(H) do { \
  if (G(H)->totalbytes >= G(H)->GCthreshold) \
  luaC_collectgarbage(H); } while (0)

#define killtemp(x) \
  check_exp(testbit((x)->gch.marked, GC_TEMP), (x)->gch.marked = 0)

#define luaC_freeall(H) luaC_sweep(H, GC_COLLECT_ALL)
LUAI_FUNC void luaC_sweep (hksc_State *H, int kind);
LUAI_FUNC void luaC_collectgarbage (hksc_State *H);
LUAI_FUNC void luaC_link (hksc_State *H, GCObject *o, lu_byte tt);
LUAI_FUNC void luaC_newcycle (hksc_State *H);
LUAI_FUNC void luaC_printstrings (hksc_State *H);


#endif
