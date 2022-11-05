/*
** $Id: ldebug.h,v 2.2 2004/06/02 19:07:55 roberto Exp roberto $
** Auxiliary functions from Debug Interface module
** See Copyright Notice in lua.h
*/

#ifndef ldebug_h
#define ldebug_h

#include "hksc_begin_code.h"

#include "lstate.h"


LUAI_FUNC void luaG_runerror (hksc_State *H, const char *fmt, ...);

#endif
