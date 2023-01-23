/*
** $Id: lmem.h $
** Interface to Memory Manager
** See Copyright Notice in lua.h
*/

#ifndef lmem_h
#define lmem_h


#include <stddef.h>

#include "llimits.h"
#include "hksclua.h"

#define MEMERRMSG  "not enough memory"


#define luaM_reallocv(H,b,on,n,e) \
  ((cast(size_t, (n)+1) <= MAX_SIZET/(e)) ?  /* +1 to avoid warnings */ \
    luaM_realloc_(H, (b), (on)*(e), (n)*(e)) : \
    luaM_toobig(H))

#define luaM_freemem(H, b, s) luaM_realloc_(H, (b), (s), 0)
#define luaM_free(H, b)   luaM_realloc_(H, (b), sizeof(*(b)), 0)
#define luaM_freearray(H, b, n, t)   luaM_reallocv(H, (b), n, 0, sizeof(t))

#define luaM_malloc(H,t)  luaM_realloc_(H, NULL, 0, (t))
#define luaM_new(H,t)   cast(t *, luaM_malloc(H, sizeof(t)))
#define luaM_newvector(H,n,t) \
    cast(t *, luaM_reallocv(H, NULL, 0, n, sizeof(t)))

#define luaM_growvector(H,v,nelems,size,t,limit,e) \
          if ((nelems)+1 > (size)) \
            ((v)=cast(t *, luaM_growaux_(H,v,&(size),sizeof(t),limit,e)))

#define luaM_reallocvector(H, v,oldn,n,t) \
   ((v)=cast(t *, luaM_reallocv(H, v, oldn, n, sizeof(t))))


LUAI_FUNC void *luaM_realloc_ (hksc_State *H, void *block, size_t oldsize,
                                                          size_t size);
LUAI_FUNC void *luaM_toobig (hksc_State *H);
LUAI_FUNC void *luaM_growaux_ (hksc_State *H, void *block, int *size,
                               size_t size_elem, int limit,
                               const char *errormsg);

#endif

