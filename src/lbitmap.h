/*
** $Id: lbitmap.h $
** creating and manipulating bitmaps
** See Copyright Notice in lua.h
*/

#ifndef lbitmap_h
#define lbitmap_h

#include "hksclua.h"

#include "llimits.h"

/* calculate the number of blocks needed for N bits */
#define bitmapnumblocks(n) cast_int((cast(unsigned int, (n)) + 31) >> 5)

typedef struct {
  lu_int32 *blocks;
  int nbits, nblocks;
} Bitmap;

#define BITMAP_INIT(b) \
  cast(void, ((b)->blocks = NULL, (b)->nbits = (b)->nblocks = 0))

/*
** test/set bit I in bitmap B
*/
#define luaO_bitmaptest(b,i) \
  ((((b)->blocks)[(i) >> 5] & (1u << ((i) & 31))) != 0)
#define luaO_bitmapset(b,i) \
  cast(void, ((b)->blocks)[(i) >> 5] |= cast(lu_int32, 1) << ((i) & 31))
#define luaO_bitmapclear(b,i) \
  cast(void, ((b)->blocks)[(i) >> 5] &= ~(cast(lu_int32, 1) << ((i) & 31)))

/*
** set bit I and return 0 if the bit was already set, 1 otherwise
*/
#define luaO_bitmapsetq(b,i) \
  (luaO_bitmaptest(b,i) ? 0 : (luaO_bitmapset(b,i), 1))

LUAI_FUNC void luaO_bitmapalloc (hksc_State *H, Bitmap *bitmap, int n);
LUAI_FUNC void luaO_bitmapfree (hksc_State *H, Bitmap *bitmap);
LUAI_FUNC int luaO_bitmapfindset (const Bitmap *bitmap, int n, int offs);
LUAI_FUNC int luaO_bitmapfindzero (const Bitmap *bitmap, int n, int offs);
LUAI_FUNC int luaO_isbitconsecutive (const Bitmap *bitmap, int bit);

#endif /* lbitmap_h */
