/*
** $Id: lbitmap.c $
** creating and manipulating bitmaps
** See Copyright Notice in lua.h
*/

#define lbitmap_c
#define LUA_CORE

#include "hksclua.h"

#include "lbitmap.h"
#include "lmem.h"

/* allocate a bitmap of N bits */
void luaO_bitmapalloc (hksc_State *H, Bitmap *bitmap, int n) {
  int oldsize = bitmap->nblocks;
  int newsize = bitmapnumblocks(n);
  bitmap->nbits = n;
  if (newsize > oldsize) {
    luaM_reallocvector(H, bitmap->blocks, oldsize, newsize, lu_int32);
    bitmap->nblocks = newsize;
  }
  while (newsize--) bitmap->blocks[newsize] = 0;
}

void luaO_bitmapfree (hksc_State *H, Bitmap *bitmap) {
  luaM_freearray(H, bitmap->blocks, bitmap->nblocks, lu_int32);
  BITMAP_INIT(bitmap);
}

/* create masks of (N % 32) bits */
#define MASK1(n) ((cast(lu_int32, 1) << ((n) & 31)) - 1)
#define MASK0(n) (~cast(lu_int32, 0) << ((n) & 31))

/*
** return the position B of the first set bit in block X
*/
static int ffs (lu_int32 x) {
  int b = 0, m = 32;  /* b = position of set bit, m = masked bits */
  while (m >>= 1)
    if ((x & MASK1(m)) == 0) x >>= m, b += m;
  return b;
}

/*
** find the first set or unset bit in the bitmap within N bits starting at OFFS
*/
static int findbit (const lu_int32 *blocks, int n, int offs, int val) {
  int i, numblocks = bitmapnumblocks(n);
  lua_assert(offs >= 0 && offs <= n);
  /* skip over completely set or unset blocks, starting at bit OFFS */
  /* OFFS+1 ensures that if (OFFS % 32) == 0, i will start at the next block,
  with the -1 correcting the extra block calculated for 32+1, while for all
  other cases, the -1 ensures i will start on the block in which OFFS resides
  */
  i = bitmapnumblocks(offs+1)-1;
  if (i < numblocks) {
    /* create a block which does not contain VAL to compare inside the loop */
    const lu_int32 no_val = cast(lu_int32, 0) - !val;
    /* the first test excludes the bits before OFFS */
    /* mask out the untested bits with !VAL, so they will not be found */
    lu_int32 b = val ? blocks[i] & MASK0(offs) : blocks[i] | MASK1(offs);
    /* skip all blocks which do not contain VAL */
    if (b == no_val) do {} while (++i < numblocks && blocks[i] == no_val);
    /* check for block partially set */
    if (i < numblocks) {
      b = val ? blocks[i] : ~blocks[i];
      /* exclude any extra bits past position N in the bitmap */
      if (i == numblocks-1 && (n & 31) && (b &= MASK1(n)) == 0)
        return -1;
      return ffs(b) + (i << 5);
    }
  }
  return -1;
}

int luaO_bitmapfindset (const Bitmap *bitmap, int n, int offs) {
  return findbit(bitmap->blocks, n, offs, 1);
}

int luaO_bitmapfindzero (const Bitmap *bitmap, int n, int offs) {
  return findbit(bitmap->blocks, n, offs, 0);
}

/*
** return true if BIT is set consecutively, meaning all bits before BIT are set
** and all bits after BIT are zero
*/
int luaO_isbitconsecutive (const Bitmap *bitmap, int bit) {
  lua_assert(bit < bitmap->nbits);
  lua_assert(luaO_bitmaptest(bitmap, bit));
  /* check that there is no zero bit in the range 0-BIT, and no 1 bit in the
     range BIT+1-N */
  return (luaO_bitmapfindzero(bitmap, bit, 0) == -1 &&
          luaO_bitmapfindset(bitmap, bitmap->nbits, bit+1) == -1);
}
