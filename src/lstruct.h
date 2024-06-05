/*
** $Id: lstruct.h $
** Auxiliary functions to manipulate structure prototypes
** See Copyright Notice in lua.h
*/

#ifndef lstruct_h
#define lstruct_h


#include "lobject.h"

#if HKSC_STRUCTURE_EXTENSION_ON

LUAI_FUNC StructProto *luaR_addstructproto(hksc_State *H, StructProto *proto);
LUAI_FUNC StructProto *luaR_getstructbyname(hksc_State *H, TString *name);
LUAI_FUNC StructProto *luaR_getstructbyid(hksc_State *H, short id);

LUAI_FUNC StructSlot *luaR_addslot(hksc_State *H, StructProto *proto,
                                   TString *name, int type, short structid);
LUAI_FUNC void luaR_addreservedslots(hksc_State *H, StructProto *proto);

LUAI_FUNC StructSlot *luaR_findslot(const StructProto *proto,
                                    const TString *name);

LUAI_FUNC lu_byte luaR_pos2index(hksc_State *H, lu_byte pos);
LUAI_FUNC lu_byte luaR_index2pos(hksc_State *H, lu_byte n);

LUAI_FUNC int luaR_compareproto(StructProto *p1, StructProto *p2);

LUAI_FUNC int luaR_checkconflicts(hksc_State *H, Table *t);
LUAI_FUNC void luaR_mergeprototypes(hksc_State *H, Table *t);

#endif /* HKSC_STRUCTURE_EXTENSION_ON */

#endif
