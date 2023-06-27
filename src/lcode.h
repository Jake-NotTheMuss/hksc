/*
** $Id: lcode.h $
** Code generator for Lua
** See Copyright Notice in lua.h
*/

#ifndef lcode_h
#define lcode_h

#include "llex.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"


/*
** Marks the end of a patch list. It is an invalid value both as an absolute
** address, and as a list link (would link an element to itself).
*/
#define NO_JUMP (-1)


/*
** grep "ORDER OPR" if you change these enums
*/
typedef enum BinOpr {
  OPR_ADD, OPR_SUB, OPR_MUL, OPR_DIV, OPR_MOD, OPR_POW,
  OPR_CONCAT,
#ifdef LUA_CODT7 /* T7 extensions */
  OPR_LEFT_SHIFT, OPR_RIGHT_SHIFT,
  OPR_BIT_AND, OPR_BIT_OR,
#endif /* LUA_CODT7 */
  OPR_NE, OPR_EQ,
  OPR_LT, OPR_LE, OPR_GT, OPR_GE,
  OPR_AND, OPR_OR,
  OPR_NOBINOPR
} BinOpr;


typedef enum UnOpr { OPR_MINUS, OPR_NOT, OPR_LEN, OPR_NOUNOPR } UnOpr;


#ifdef LUA_CODIW6
/*
** hdelete expression kinds
*/
enum DELETE_KIND {
  DELETE_LOCAL,  /* delete a local variable */
  DELETE_GLOBAL,  /* delete a global variable */
  DELETE_UPVAL,  /* delete an upvalue */
  DELETE_INDEXED,  /* delete a table index */
  DELETE_MAX
};
#endif /* LUA_CODIW6 */


/*
** kinds of slot assignments in a VSLOT store opeation; these kinds determine
** what code is generated for the store
*/
typedef enum SlotAssignmentKind {
  ASSIGN_SLOT_NIL,  /* a `nil' slot assignment */
  ASSIGN_SLOT_STATIC,  /* a statically typed slot assignment */
  ASSIGN_SLOT_DYNAMIC,  /* a slot assignment that is type-checked at runtime */
  ASSIGN_SLOT_GENERIC,  /* an assignment to an index that may resolve to a
                           structure slot at runtime */
  ASSIGN_SLOT_PROXYTABLE  /* an assignment to the backing table (for indices
                             which cannot resolve to slots) */
} SlotAssignmentType;


#define getcode(fs,e)	((fs)->f->code[(e)->u.s.info])

#define luaK_codeAsBx(fs,o,A,sBx)	luaK_codeABx(fs,o,A,(sBx)+MAXARG_sBx)

#define luaK_setmultret(fs,e)	luaK_setreturns(fs, e, LUA_MULTRET)

LUAI_FUNC int luaK_codeABx (FuncState *fs, OpCode o, int A, unsigned int Bx);
LUAI_FUNC int luaK_codeABC (FuncState *fs, OpCode o, int A, int B, int C);
LUAI_FUNC void luaK_fixline (FuncState *fs, int line);
LUAI_FUNC void luaK_nil (FuncState *fs, int from, int n);
LUAI_FUNC void luaK_reserveregs (FuncState *fs, int n);
LUAI_FUNC void luaK_checkstack (FuncState *fs, int n);
LUAI_FUNC int luaK_stringK (FuncState *fs, TString *s);
LUAI_FUNC int luaK_numberK (FuncState *fs, lua_Number r);
LUAI_FUNC int luaK_literalK(FuncState *fs, lu_int64 l, int type);
LUAI_FUNC void luaK_dischargevars (FuncState *fs, expdesc *e);
LUAI_FUNC int luaK_exp2anyreg (FuncState *fs, expdesc *e);
LUAI_FUNC void luaK_exp2nextreg (FuncState *fs, expdesc *e);
LUAI_FUNC void luaK_exp2val (FuncState *fs, expdesc *e);
LUAI_FUNC int luaK_exp2RK (FuncState *fs, expdesc *e);
LUAI_FUNC void luaK_self (FuncState *fs, expdesc *e, expdesc *key);
LUAI_FUNC void luaK_indexed (FuncState *fs, expdesc *t, expdesc *k);
LUAI_FUNC void luaK_goiftrue (FuncState *fs, expdesc *e);
#if HKSC_STRUCTURE_EXTENSION_ON
LUAI_FUNC void luaK_setslot (FuncState *fs, StructSlot *slot, const TypeInfo *t,
                             int reg, int key, int val, int kind);
LUAI_FUNC int luaK_checkslotassignment (FuncState *fs, TString *name,
                                        expdesc *e, const TypeInfo *t);
LUAI_FUNC void luaK_getslottypeinfo (FuncState *fs, StructSlot *slot,
                                     TypeInfo *t);
LUAI_FUNC void luaK_checktype (FuncState *fs, TypeInfo *t, int reg);
LUAI_FUNC void luaK_applytypecontraint (FuncState *fs, TypeInfo *t, expdesc *e);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
LUAI_FUNC void luaK_storevar (FuncState *fs, expdesc *var, expdesc *e);
#ifdef LUA_CODIW6
LUAI_FUNC void luaK_delete (FuncState *fs, expdesc *var);
#endif /* LUA_CODIW6 */
LUAI_FUNC void luaK_setreturns (FuncState *fs, expdesc *e, int nresults);
LUAI_FUNC void luaK_setoneret (FuncState *fs, expdesc *e);
LUAI_FUNC int luaK_jump (FuncState *fs);
LUAI_FUNC void luaK_ret (FuncState *fs, int first, int nret);
LUAI_FUNC void luaK_patchlist (FuncState *fs, int list, int target);
LUAI_FUNC void luaK_patchtohere (FuncState *fs, int list);
LUAI_FUNC void luaK_concat (FuncState *fs, int *l1, int l2);
LUAI_FUNC int luaK_getlabel (FuncState *fs);
LUAI_FUNC void luaK_prefix (FuncState *fs, UnOpr op, expdesc *v);
LUAI_FUNC void luaK_infix (FuncState *fs, BinOpr op, expdesc *v);
LUAI_FUNC void luaK_posfix (FuncState *fs, BinOpr op, expdesc *v1, expdesc *v2);
LUAI_FUNC void luaK_setlist (FuncState *fs, int base, int nelems, int tostore);
LUAI_FUNC void luaK_optimize_function (hksc_State *H, Proto *f);


#endif
