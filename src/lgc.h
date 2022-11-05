/*
** $Id: lgc.h,v 2.14 2005/06/07 18:53:45 roberto Exp roberto $
** Garbage Collector
** See Copyright Notice in lua.h
*/

#ifndef lgc_h
#define lgc_h


#include "lobject.h"


/*
** Possible states of the Garbage Collector
*/
#define GCSpause  0
#define GCSpropagate  1
#define GCSsweepstring  2
#define GCSsweep  3


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
** bit 0 - object is alive
** bit 1 - object is fixed (should not be collected)
** bit 2 - object is "super" fixed (only the main thread)
*/


#define LIVEBIT 0
#define FIXEDBIT  1
#define SFIXEDBIT 2

#define islive(g,x)       testbit((x)->gch.marked, LIVEBIT)
#define isdead(g,x)       (!islive(g,x))
#define isfixed(g,x)      testbit((x)->gch.marked, FIXEDBIT)

#define otherwhite(g) (g->currentwhite ^ bitmask(LIVEBIT))

#define makelive(x)  l_setbit((x)->gch.marked, LIVEBIT)
#define makedead(x)  resetbit((x)->gch.marked, LIVEBIT)

#define luaC_live(g) cast(lu_byte, bitmask(LIVEBIT))


#define luaC_checkGC(H) { \
  /*if (G(H)->totalbytes >= G(H)->GCthreshold) */\
  luaC_step(H); }

LUAI_FUNC void luaC_freeall (hksc_State *H);
LUAI_FUNC void luaC_step (hksc_State *H);
LUAI_FUNC void luaC_fullgc (hksc_State *H);
LUAI_FUNC void luaC_link (hksc_State *H, GCObject *o, lu_byte tt);
LUAI_FUNC void luaC_newcycle (hksc_State *H);


#endif
