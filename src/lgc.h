/*
** $Id: lgc.h $
** Garbage Collector
** See Copyright Notice in lua.h
*/

/*******************************************************************************
  The garbage collector was simplified for the needs of hksc. It functions more
  like the Lua 5.0 collector. There are 4 object types that are used by the
  library, strings, tables, prototypes, and analyzers.

  Like in Lua, strings that are needed for the entire duration of the Lua state
  can be fixed (marked with FIXEDBIT) and can thus avoid normal collection.

  A new mark is added, TEMPBIT, which is used for tables, prototypes, and
  analyzers. Objects of these types only ever last one parser cycle, and
  therefore can always be collected at the start of the next cycle, without
  needing to mark them. Similarly, non-fixed strings could also be collected on
  each new cycle, but instead they are only marked, and then maybe collected if
  the total memory usage is above the given threshold. The reasoning is that
  multiple Lua files may use the same strings (e.g. in using the same global
  variables), and so not collecting them every cycle avoids having to
  continuously deallocate and reallocate memory for the same string.
*******************************************************************************/

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
** bit 0 - object is alive (used for strings)
** bit 1 - object is temporary (all tables and prototypes are temporary)
** bit 2 - object is fixed (should not be collected)
** bit 3 - object is "super" fixed (only the main thread)
*/


#define LIVEBIT 0
#define TEMPBIT 1
#define FIXEDBIT  2
#define SFIXEDBIT 3

#define islive(g,x)       ((void)(g), testbit((x)->gch.marked, LIVEBIT))
#define istemp(g,x)       ((void)(g), testbit((x)->gch.marked, TEMPBIT))
#define isfixed(g,x)      ((void)(g), testbit((x)->gch.marked, FIXEDBIT))

#define otherwhite(g) (g->currentwhite ^ LIVEMASK)
#define isdead(g,v) ((void)(g), !islive(g,v))

#define makelive(x)  l_setbit((x)->gch.marked, LIVEBIT)
#define makedead(x)  resetbit((x)->gch.marked, LIVEBIT)

#define luaC_white(g) ((void)(g), cast(lu_byte, LIVEMASK))

#define LIVEMASK  bitmask(LIVEBIT)

#define luaC_checkGC(H) do { \
  if (G(H)->totalbytes >= G(H)->GCthreshold) \
  luaC_collectgarbage(H); } while (0)

#define killtemp(x)  \
  check_exp(testbit((x)->gch.marked, TEMPBIT), (x)->gch.marked = 0)

LUAI_FUNC void luaC_freeall (hksc_State *H);
LUAI_FUNC void luaC_collectgarbage (hksc_State *H);
LUAI_FUNC void luaC_link (hksc_State *H, GCObject *o, lu_byte tt);
LUAI_FUNC void luaC_newcycle (hksc_State *H);
LUAI_FUNC void luaC_printstrings (hksc_State *H);


#endif
