/*
** $Id: lvec.h $
** Macros to define and create vectors of element type T
** See Copyright Notice in lua.h
*/

#ifndef lvec_h
#define lvec_h

#include "hksclua.h"

#include "llimits.h"
#include "lmem.h"

/* declare a variable of structure type which describes a vector of T */
#define VEC_DECL(T, name) struct { T *s; int used, alloc; } name

/* initialize a vector struct */
#define VEC_INIT(v)  cast(void, ((v).s = NULL, (v).used = (v).alloc = 0))

/* ensure there is space for one more element in vector struct V */
#define VEC_GROW(H, v) \
  cast(void, ((v).used+1 > (v).alloc) ? \
       (v).s=luaM_growaux_(H, (v).s, &(v).alloc, sizeof((v).s[0]), \
                           MAX_INT, "") : \
       0)

/* returns a pointer to the next free element, increases the element coumnt */
#define VEC_NEWELT(H, v)  (VEC_GROW(H, v), &(v).s[(v).used++])

/* free a vector */
#define VEC_FREE(H, v) \
  cast(void, luaM_freemem(H, (v).s, (v).alloc * sizeof((v).s[0])))

#endif /* lvec_H */
