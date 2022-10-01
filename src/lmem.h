/*
** $Id: lmem.h,v 1.30 2005/03/18 16:38:02 roberto Exp roberto $
** Interface to Memory Manager
** See Copyright Notice in lua.h
*/

#ifndef lmem_h
#define lmem_h

#include "hksc_begin_code.h"

#include <stddef.h>

#include "llimits.h"
#include "lua.h"

#define MEMERRMSG  "not enough memory"


#define luaM_reallocv(b,on,n,e) \
  ((cast(size_t, (n)+1) <= MAX_SIZET/(e)) ?  /* +1 to avoid warnings */ \
    luaM_realloc_((b), (on)*(e), (n)*(e)) : \
    luaM_toobig())

#define luaM_freemem(b, s)  luaM_realloc_((b), (s), 0)
#define luaM_free(b)    luaM_realloc_((b), sizeof(*(b)), 0)
#define luaM_freearray(b, n, t)   luaM_reallocv((b), n, 0, sizeof(t))

#define luaM_malloc(t)  luaM_realloc_(NULL, 0, (t))
#define luaM_new(t)    cast(t *, luaM_malloc(sizeof(t)))
#define luaM_newvector(n,t) \
    cast(t *, luaM_reallocv(NULL, 0, n, sizeof(t)))

#define luaM_growvector(v,nelems,size,t,limit,e) \
          if ((nelems)+1 > (size)) \
            ((v)=cast(t *, luaM_growaux_(v,&(size),sizeof(t),limit,e)))

#define luaM_reallocvector(v,oldn,n,t) \
   ((v)=cast(t *, luaM_reallocv(v, oldn, n, sizeof(t))))


LUAI_FUNC void *luaM_realloc_ (void *block, size_t oldsize,
                                                          size_t size);
LUAI_FUNC void *luaM_toobig (void);
LUAI_FUNC void *luaM_growaux_ (void *block, int *size,
                               size_t size_elem, int limit,
                               const char *errormsg);

#endif

