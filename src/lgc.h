/*
** $Id: lgc.h $
** Garbage Collector
** See Copyright Notice in lua.h
*/

/*******************************************************************************
  The garbage collector was heavily simplified for the needs of hksc. There are
  3 object types that are used by the library: strings, tables, and prototypes.

  Like in Lua, strings that are needed for the entire duration of the Lua state
  can be fixed (marked with FIXEDBIT) and can thus avoid normal collection.

  Notably, there is now no need for `white', `gray', and `black' descriptors, as
  traversable objects in hksc (tables and prototypes) are, from the collector's
  perspective, already dead from the moment they are created.

  Only strings have a `liveness', whereas tables and prototypes are given a new
  mark, TEMPBIT. Tables and prototypes are marked as temporary because they are
  only needed during a single compilation procedure. Afterward, they can not
  serve a further purpose. Thus, they are always collected before each new
  compilation procedure begins, which is when the collector performs a cycle.

  Along with collecting all temporary objects, the collector also marks all
  non-fixed strings for collection on each cycle, in the propogation phase. The
  collector will then perform a string sweep if total memory usage is above a
  given threshold.
*******************************************************************************/

#ifndef lgc_h
#define lgc_h


#include "lobject.h"


/*
** Possible states of the Garbage Collector
*/
#define GCSpause        0 /* inactive */
#define GCSmarking      1 /* marking strings */
#define GCSsweep        2 /* sweeping temp list */
#define GCSsweepstring  3 /* sweeping string lists */


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
