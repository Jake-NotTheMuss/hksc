/*
** $Id: lprintk.h
** print constants
** See Copyright Notice in lua.h
*/

#ifndef lprintk_h
#define lprintk_h

#include "hksclua.h"

typedef void (*l_PFN) (const char *s, size_t n, void *ud);

#include "lobject.h"

LUAI_FUNC void luaO_printstring (const TString *ts, l_PFN pfn, void *ud,
                                 int quote);
LUAI_FUNC void luaO_printk (const TValue *o, l_PFN pfn, void *ud, int quote);

#endif /* lprintk_h */
