/*
** $Id: ldebug.h $
** Auxiliary functions from Debug Interface module
** See Copyright Notice in lua.h
*/

#ifndef ldebug_h
#define ldebug_h

#include "lstate.h"


#define pcRel(pc, p)  (cast(int, (pc) - (p)->code) - 1)

#define getline(f,pc) (((f)->lineinfo) ? (f)->lineinfo[pc] : 0)


LUAI_FUNC void luaG_runerror (hksc_State *H, const char *fmt, ...);
LUAI_FUNC int luaG_checkcode (const Proto *pt);
LUAI_FUNC int luaG_checkopenop (Instruction i);

#endif
