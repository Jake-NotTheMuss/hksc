/*
** $Id: lcode.c $
** Code generator for Lua
** See Copyright Notice in lua.h
*/


#include <stdlib.h>
#include <string.h> /* memset */

#define lcode_c
#define LUA_CORE

#include "hksclua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstate.h"
#include "lstruct.h"
#include "ltable.h"


#define hasjumps(e)	((e)->t != (e)->f)


static int isnumeral(expdesc *e) {
  return (e->k == VKNUM && e->t == NO_JUMP && e->f == NO_JUMP);
}


static int ismatchingtype(expdesc *e1, expdesc *e2) {
  if (!expisstruct(e1)) lua_assert(expproto(e1) == NULL);
  if (!expisstruct(e2)) lua_assert(expproto(e2) == NULL);
  return(exptype(e1) == exptype(e2) && expproto(e1) == expproto(e2));
}


void luaK_nil (FuncState *fs, int from, int n) {
  Instruction *previous;
  /* no jumps to current position? */
  if (fs->pc > fs->lasttarget && fs->pc != 0) {
    if (GET_OPCODE(*(previous = &fs->f->code[fs->pc-1])) == OP_LOADNIL) {
      int pfrom = GETARG_A(*previous);
      int pto = GETARG_B(*previous);
      if (pfrom <= from && from <= pto+1) {  /* can connect both? */
        if (from+n-1 > pto)
          SETARG_B(*previous, from+n-1);
        return;
      }
    }
  }
#ifdef HKSC_TEST_WITH_STANDARD_LUA
  else if (fs->pc > fs->lasttarget && fs->pc == 0)
    return;  /* positions are already clean */
#endif  /* HKSC_TEST_WITH_STANDARD_LUA */
  luaK_codeABC(fs, OP_LOADNIL, from, from+n-1, 0);  /* else no optimization */
}


int luaK_jump (FuncState *fs) {
  int jpc = fs->jpc;  /* save list of jumps to here */
  int j;
  fs->jpc = NO_JUMP;
  j = luaK_codeAsBx(fs, OP_JMP, 0, NO_JUMP);
  luaK_concat(fs, &j, jpc);  /* keep them on hold */
  return j;
}


void luaK_ret (FuncState *fs, int first, int nret) {
  luaK_codeABC(fs, OP_RETURN, first, nret+1, 0);
}


static int condjump (FuncState *fs, OpCode op, int A, int B, int C) {
  luaK_codeABC(fs, op, A, B, C);
  return luaK_jump(fs);
}


static void fixjump (FuncState *fs, int pc, int dest) {
  Instruction *jmp = &fs->f->code[pc];
  int offset = dest-(pc+1);
  lua_assert(dest != NO_JUMP);
  if (abs(offset) > MAXARG_sBx)
    luaX_inputerror(fs->ls,
                    "Control structure contains too many instructions.");
  SETARG_sBx(*jmp, offset);
}


/*
** returns current `pc' and marks it as a jump target (to avoid wrong
** optimizations with consecutive instructions not in the same basic block).
*/
int luaK_getlabel (FuncState *fs) {
  fs->lasttarget = fs->pc;
  return fs->pc;
}


static int getjump (FuncState *fs, int pc) {
  int offset = GETARG_sBx(fs->f->code[pc]);
  if (offset == NO_JUMP)  /* point to itself represents end of list */
    return NO_JUMP;  /* end of list */
  else
    return (pc+1)+offset;  /* turn offset into absolute position */
}


static Instruction *getjumpcontrol (FuncState *fs, int pc) {
  Instruction *pi = &fs->f->code[pc];
  if (pc >= 1 && testTMode(GET_OPCODE(*(pi-1))))
    return pi-1;
  else
    return pi;
}


/*
** check whether list has any jump that do not produce a value
** (or produce an inverted value)
*/
static int need_value (FuncState *fs, int list) {
  for (; list != NO_JUMP; list = getjump(fs, list)) {
    Instruction i = *getjumpcontrol(fs, list);
    if (GET_OPCODE(i) != OP_TESTSET) return 1;
  }
  return 0;  /* not found */
}


static int patchtestreg (FuncState *fs, int node, int reg) {
  Instruction *i = getjumpcontrol(fs, node);
  if (GET_OPCODE(*i) != OP_TESTSET)
    return 0;  /* cannot patch other instructions */
  if (reg != NO_REG && reg != GETARG_B(*i))
    SETARG_A(*i, reg);
  else  /* no register to put value or register already has the value */
    *i = CREATE_ABC(OP_TEST, GETARG_B(*i), 0, GETARG_C(*i));

  return 1;
}


static void removevalues (FuncState *fs, int list) {
  for (; list != NO_JUMP; list = getjump(fs, list))
      patchtestreg(fs, list, NO_REG);
}


static void patchlistaux (FuncState *fs, int list, int vtarget, int reg,
                          int dtarget) {
  while (list != NO_JUMP) {
    int next = getjump(fs, list);
    if (patchtestreg(fs, list, reg))
      fixjump(fs, list, vtarget);
    else
      fixjump(fs, list, dtarget);  /* jump to default target */
    list = next;
  }
}


static void dischargejpc (FuncState *fs) {
  patchlistaux(fs, fs->jpc, fs->pc, NO_REG, fs->pc);
  fs->jpc = NO_JUMP;
}


void luaK_patchlist (FuncState *fs, int list, int target) {
  if (target == fs->pc)
    luaK_patchtohere(fs, list);
  else {
    lua_assert(target < fs->pc);
    patchlistaux(fs, list, target, NO_REG, target);
  }
}


void luaK_patchtohere (FuncState *fs, int list) {
  luaK_getlabel(fs);
  luaK_concat(fs, &fs->jpc, list);
}


void luaK_concat (FuncState *fs, int *l1, int l2) {
  if (l2 == NO_JUMP) return;
  else if (*l1 == NO_JUMP)
    *l1 = l2;
  else {
    int list = *l1;
    int next;
    while ((next = getjump(fs, list)) != NO_JUMP)  /* find last element */
      list = next;
    fixjump(fs, list, l2);
  }
}


void luaK_checkstack (FuncState *fs, int n) {
  int newstack = fs->freereg + n;
  if (newstack > fs->f->maxstacksize) {
    if (newstack >= MAXSTACK)
      luaX_inputerror(fs->ls, "Function or expression requires too many "
                      "registers (too complex).");
    fs->f->maxstacksize = cast_byte(newstack);
  }
}


void luaK_reserveregs (FuncState *fs, int n) {
  luaK_checkstack(fs, n);
  fs->freereg += n;
}


static void freereg (FuncState *fs, int reg) {
  if (!ISK(reg) && reg >= fs->nactvar) {
    fs->freereg--;
    lua_assert(reg == fs->freereg);
  }
}


static void freeexp (FuncState *fs, expdesc *e) {
  if (e->k == VNONRELOC)
    freereg(fs, e->u.s.info);
}


#if HKSC_GETGLOBAL_MEMOIZATION
static int addmemoslot (FuncState *fs, int kslot) {
  hksc_State *H = fs->H;
  Proto *f = fs->f;
  TValue key, *idx;
  sethlvalue(&key, cast(size_t, kslot));
  idx = luaH_set(H, fs->h, &key);
  if (ttislightuserdata(idx)) {
    return cast_int(hlvalue(idx));
  }
  else {
    int memoslot = fs->nk, oldsize = f->sizek;
    sethlvalue(idx, cast(size_t, memoslot));
    /* reserve 2 more slots */
    if (fs->nk+2 > f->sizek)
      f->k = luaM_growaux_(H, f->k, &f->sizek, sizeof(TValue), MAXARG_Bx,
                           "constant table overflow");
    while (oldsize < f->sizek) setnilvalue(&f->k[oldsize++]);
    fs->nk += 2;
    return memoslot;
  }
}
#endif /* HKSC_GETGLOBAL_MEMOIZATION */


static int addk (FuncState *fs, TValue *k, TValue *v) {
  hksc_State *H = fs->H;
  TValue *idx = luaH_set(H, fs->h, k);
  Proto *f = fs->f;
  int oldsize = f->sizek;
  if (ttisnumber(idx)) {
    lua_assert(luaO_rawequalObj(&fs->f->k[cast_int(nvalue(idx))], v));
    return cast_int(nvalue(idx));
  }
  else {  /* constant not found; create a new entry */
    setnvalue(idx, cast_num(fs->nk));
    luaM_growvector(H, f->k, fs->nk, f->sizek, TValue,
                    MAXARG_Bx, "constant table overflow");
    while (oldsize < f->sizek) setnilvalue(&f->k[oldsize++]);
    setobj(&f->k[fs->nk], v);
    /*luaC_barrier(L, f, v);*/
    return fs->nk++;
  }
}


int luaK_stringK (FuncState *fs, TString *s) {
  TValue o;
  setsvalue(&o, s);
  return addk(fs, &o, &o);
}


int luaK_numberK (FuncState *fs, lua_Number r) {
  TValue o;
  setnvalue(&o, r);
  return addk(fs, &o, &o);
}


int luaK_literalK(FuncState *fs, lu_int64 l, int type)
{
  TValue o;
  lua_assert(type == TK_LITERALLUD || type == TK_LITERALUI64);
  if (type == TK_LITERALLUD &&
      (hksc_getintliteralsenabled(fs->H) & INT_LITERALS_LUD))
    sethlvalue(&o, lua_ui64tolud(l));
  else if (type == TK_LITERALUI64) {
    if ((hksc_getintliteralsenabled(fs->H) & INT_LITERALS_UI64) == 0)
      goto literals_not_enabled;
#ifdef HKSC_UI64API
    else
      setui64value(&o, l);
#else /* !HKSC_UI64API */
#ifdef HKSC_MATCH_HAVOKSCRIPT_ERROR_MSG
    luaX_inputerror(fs->ls,"60-bit literal not supported without HKS_UI64API");
#else /* !HKSC_MATCH_HAVOKSCRIPT_ERROR_MSG */
    luaX_inputerror(fs->ls,
                    "60-bit literal not supported without HKSC_UI64API");
#endif /* HKSC_MATCH_HAVOKSCRIPT_ERROR_MSG */
#endif /* HKSC_UI64API */
  }
  else {
    literals_not_enabled:
    luaX_syntaxerror(fs->ls, "int literals not enabled in compiler options");
  }
  return addk(fs, &o, &o);
}


static int boolK (FuncState *fs, int b) {
  TValue o;
  setbvalue(&o, b);
  return addk(fs, &o, &o);
}


static int nilK (FuncState *fs) {
  TValue k, v;
  setnilvalue(&v);
  /* cannot use nil as key; instead use table itself to represent nil */
  sethvalue(&k, fs->h);
  return addk(fs, &k, &v);
}


void luaK_setreturns (FuncState *fs, expdesc *e, int nresults) {
  if (e->k == VCALL) {  /* expression is an open function call? */
    SETARG_C(getcode(fs, e), nresults+1);
  }
  else if (e->k == VVARARG) {
    SETARG_B(getcode(fs, e), nresults+1);
    SETARG_A(getcode(fs, e), fs->freereg);
    luaK_reserveregs(fs, 1);
  }
}


void luaK_setoneret (FuncState *fs, expdesc *e) {
  if (e->k == VCALL) {  /* expression is an open function call? */
    e->k = VNONRELOC;
    e->u.s.info = GETARG_A(getcode(fs, e));
  }
  else if (e->k == VVARARG) {
    SETARG_B(getcode(fs, e), 2);
    e->k = VRELOCABLE;  /* can relocate its simple result */
  }
}


#if HKSC_STRUCTURE_EXTENSION_ON

#define STRUCT_MAX_TAG_CHAIN  5
#define STRUCT_LOOKUP_MAX_STEPS  15

/*
** kinds of struct index for the code generator; controls what code is
** generated for struct indexes
*/
enum StructIndexKind {
  STRUCT_INDEX_SLOT,  /* a valid slot in the structure */
  STRUCT_INDEX_PROXYTABLE, /* slot was not resolved, index the backing table */
  STRUCT_INDEX_INVALID  /* an invalid index */
};


typedef struct StructSlotLookupStep {
  StructProto *p;  /* the structure holding the slot that was searched for */
  StructSlot *s;  /* the slot searched for */
  int istagstep;  /* true if this lookup was done to access a prototype
                     contained by a tag-method slot */
} StructSlotLookupStep;


typedef struct StructSlotLookup {
  /* the slot type info; `is_static' in this context refers to the consistency
     of the slot type across the tag chain */
  TypeInfo t;
  /* the tag-method slot to search for when tarversing up the meta chain,
     either `__index' or `__newindex' */
  TString *tm_search_name;
  /* number of slot lookups so far */
  int nsteps;
  /* whether assigning a value to the slot or retrieving the value */
  int store;
  /* array of all slot lookups in order */
  StructSlotLookupStep steps[STRUCT_LOOKUP_MAX_STEPS];
} StructSlotLookup;


static int lookup_step (StructSlotLookup *s, StructProto *proto,
                        StructSlot *slot, int istagstep) {
  if (s->nsteps < STRUCT_LOOKUP_MAX_STEPS) {
    StructSlotLookupStep *step = &s->steps[s->nsteps++];
    step->p = proto;
    step->s = slot;
    step->istagstep = istagstep;
    return 1;
  }
  return 0;
}


static void init_struct_lookup (FuncState *fs, StructSlotLookup *s, int set) {
  global_State *g = G(fs->H);
  s->t.type = LUA_TNONE;
  s->t.proto = NULL;
  s->t.is_static = 1;
  s->tm_search_name = set ? g->tm_names[TM_NEWINDEX] : g->tm_names[TM_INDEX];
  s->store = set;
  s->nsteps = 0;
}


/*
** populate the TypeInfo T for a given SLOT of a structure prototype
*/
void luaK_getslottypeinfo (FuncState *fs, StructSlot *slot, TypeInfo *t) {
  StructProto *proto = NULL;
  int type = slot->typeid;
  if (type == LUA_TNIL) type = LUA_TNONE;
  if (type == LUA_TSTRUCT)
    proto = luaR_getstructbyid(fs->H, slot->structid);
  if (t->type != LUA_TNONE && t->is_static)
    t->is_static = (t->type == type && t->proto == proto);
  t->type = type;
  t->proto = proto;
}


/*
** resolve a structure index as a primitive lookup, i.e. without involving meta
** prototypes
*/
static int resolveprimitive (FuncState *fs, StructProto *proto, TString *name,
                             StructSlotLookup *s) {
  StructSlot *slot = luaR_findslot(proto, name);
  if (slot != NULL) {
    if (!lookup_step(s, proto, slot, 0))
      /* slot exists but there are no more steps left to access it; a normal
         table lookup has to be emitted */
      return STRUCT_INDEX_PROXYTABLE;
    /* update the type info for the slot */
    luaK_getslottypeinfo(fs, slot, &s->t);
    return STRUCT_INDEX_SLOT;
  }
  else {  /* slot not found */
    if (proto->hasproxy)
      return STRUCT_INDEX_PROXYTABLE;  /* use the backing table */
    lookup_step(s, proto, NULL, 0);  /* an initial lookup to not find NAME */
    /* there is no backing table, so the index is invalid */
    return STRUCT_INDEX_INVALID;
  }
}


static int resolvestructindex (FuncState *fs, StructProto *proto,
                               TString *name, StructSlotLookup *s) {
  int result1 = resolveprimitive(fs, proto, name, s);
  if (result1 == STRUCT_INDEX_PROXYTABLE) {
    s->t.type = LUA_TNONE;
    /* the backing table will be used for the lookup, nothing more to do */
    return STRUCT_INDEX_PROXYTABLE;
  }
  if (proto->hasmeta) {
    /* save the current state; if the meta slot does not need to be accessed
       because it doesn't have a tag-mathod slot or the structure type said
       slot contains does not have the desired slot, the state of the lookup
       will be reverted back to this value */
    int nsteps = s->nsteps;
    StructSlot *metaslot = getmetaslot(proto);
    /* the structure contained by the meta slot if it is a structure type */
    StructProto *metaproto;
    /* the `__index' or `__newindex' slot */
    StructSlot *tmslot;
    /* a lookup to access the meta slot */
    if (!lookup_step(s, proto, metaslot, 1) || metaslot->typeid != LUA_TSTRUCT)
      /* the meta slot may not contain a structure; GETTABLE is required */
      return STRUCT_INDEX_PROXYTABLE;
    /* meta slot is a structure type; get the prototype */
    metaproto = luaR_getstructbyid(fs->H, metaslot->structid);
    lua_assert(metaproto != NULL);
    /* find the tag-method slot in the meta slot structure type */
    tmslot = luaR_findslot(metaproto, s->tm_search_name);
    if (tmslot != NULL) {
      /* structure contained by the tag-method slot if it is a structure
         type */
      StructProto *parent;
      int result;  /* result of recursive call */
      /* a lookup to access the tag-method slot */
      if (!lookup_step(s, metaproto, tmslot, 1) ||
          tmslot->typeid != LUA_TSTRUCT)
        /* the tag-method slot is not a structure type; GETTABLE is required */
        return STRUCT_INDEX_PROXYTABLE;
      parent = luaR_getstructbyid(fs->H, tmslot->structid);
      lua_assert(parent != NULL);
      /* look up NAME in the parent structure prototype */
      result = resolvestructindex(fs, parent, name, s);
      /* an invalid result marks the end of recursion, because it means this is
         the last valid structure prototype that could be accessed in the
         tag-chain; therefore, the result is only passed to the caller if it is
         a valid result, otherwise the terminal logic is run */
      if (result != STRUCT_INDEX_INVALID)
        return result;
      else if (s->store)
        /* for slot assignments, when the `__newindex' slot contains a
           structure, but the dseired slot cannot be resolved in that
           structure, SETTABLE is emitted */
        return STRUCT_INDEX_PROXYTABLE;
      /* else go through and handle the terminal case */
    }
    if (metaproto->hasproxy)
      /* given now that the tag-method slot either does not exist or cannot be
         used to access the desired slot, the meta prototype backing table is
         next in line to fall back on */
      return STRUCT_INDEX_PROXYTABLE;
    /* there is no backing table and the meta slot does not yield any access to
       the desired slot, so it will not be used for lookup; revert back to the
       state before accessing the meta */
    s->nsteps = nsteps;
  }
  return result1;  /* return result of primitive lookup */
}


static int resolvenewstructindex (FuncState *fs, StructProto *proto,
                                  TString *name, StructSlotLookup *s) {
  int result = resolvestructindex(fs, proto, name, s);
  /* if the slot is valid, but the type is inconsistent between parent/child
     structures, use SETTABLE, as the instruction sequence used for slot
     assignments will not be sufficient to ensure types are properly checked */
  if (result == STRUCT_INDEX_SLOT && s->t.is_static == 0)
    result = STRUCT_INDEX_PROXYTABLE;
  return result;
}


/*
** emit data opcodes to encode a complete tag-chain for a structure index
*/
static void codetagchain (FuncState *fs, StructSlotLookup *s) {
  int i;
  for (i = 0; i/3 < s->nsteps/3; i+=3) {
    StructSlot *slot = s->steps[i].s;
    StructSlot *tmslot = s->steps[i+2].s;  /* `__index' slot */
    lu_byte slotposition = slot ? slot->position : 0;
    lua_assert(tmslot != NULL);
    luaK_codeABx(fs, OP_DATA, slotposition, tmslot->position);
  }
  lua_assert(s->steps[s->nsteps-1].s != NULL);
  luaK_codeABx(fs, OP_DATA, s->steps[s->nsteps-1].s->position, 0);
}


static void invalid_index (FuncState *fs, StructProto *proto, TString *name) {
  const char *msg = luaO_pushfstring(fs->H, "Cannot index instances of '%s' "
                          "with '%s'", getstr(proto->name), getstr(name));
  luaX_semerror(fs->ls, msg);
}

static void cannot_resolve (FuncState *fs, StructProto *proto, TString *name) {
  const char *msg = luaO_pushfstring(fs->H, "Cannot resolve slot '%s' in "
      "instance of '%s' for assignment.", getstr(name), getstr(proto->name));
  luaX_semerror(fs->ls, msg);
}


static int code_getslot (FuncState *fs, expdesc *e, StructProto *proto,
                     TString *slotname, int selfreg) {
  StructSlotLookup s;
  int pc, dest, result;
  OpCode op, op_mt;
  if (selfreg != NO_REG) {
    op = OP_SELFSLOT; op_mt = OP_SELFSLOTMT; dest = selfreg;
  }
  else {
    op = OP_GETSLOT; op_mt = OP_GETSLOTMT; dest = 0;
  }
  init_struct_lookup(fs, &s, 0);
  lua_assert(proto != NULL);
  result = resolvestructindex(fs, proto, slotname, &s);
  if (result == STRUCT_INDEX_SLOT) {
    if (s.nsteps == 1) {
      StructSlot *slot = s.steps[0].s;
      lua_assert(slot != NULL);
      pc = luaK_codeABC(fs, op, dest, e->u.s.info, slot->position);
    }
    else {
      pc = luaK_codeABC(fs, op_mt, dest, e->u.s.info, s.nsteps/3);
      codetagchain(fs, &s);
    }
    return pc;
  }
  else if (result == STRUCT_INDEX_INVALID)
    invalid_index(fs, proto, slotname);
  return -1;
}


/*
** call this for primitive slot assignments; KIND controls what store code gets
** generated for the assignment; KEY and VAL are RK-values for the key and
** value in the assignment; REG is the struct; T has the slot type info, SLOT
** may be NULL if assigning to the proxytable
*/
void luaK_setslot (FuncState *fs, StructSlot *slot, const TypeInfo *t, int reg,
                   int key, int val, int kind) {
  if (kind != ASSIGN_SLOT_GENERIC && kind != ASSIGN_SLOT_PROXYTABLE)
    lua_assert(slot != NULL);
  switch (kind) {
    case ASSIGN_SLOT_NIL:
      luaK_codeABC(fs, OP_SETSLOTN, reg, 0, slot->position);
      break;
    case ASSIGN_SLOT_STATIC:
      luaK_codeABC(fs, OP_SETSLOTI, reg, slot->position, val);
      break;
    case ASSIGN_SLOT_DYNAMIC: {
      int bx;  /* type id or struct id to check */
      if (t->type == LUA_TSTRUCT) {
        lua_assert(t->proto != NULL);
        luaK_codeABC(fs, OP_SETSLOTS, reg, slot->position, val);
        bx = cast_int(t->proto->id);
      }
      else {
        luaK_codeABC(fs, OP_SETSLOT, reg, slot->position, val);
        bx = t->type;
      }
      luaK_codeABx(fs, OP_DATA, 0, bx);
      break;
    }
    default:
      luaK_codeABC(fs, OP_SETTABLE, reg, key, val);
      break;
  }
}


static int assignslot (FuncState *fs, expdesc *var, expdesc *e, int reg) {
  StructSlotLookup s;
  TString *slotname;
  StructProto *proto = var->u.s.proto;
  int res;
  lua_assert(var->k == VSLOT);
  if (!ttisstring(&var->u.s.structindex))
    return 0;  /* index must be a string to be a slot */
  slotname = rawtsvalue(&var->u.s.structindex);
  init_struct_lookup(fs, &s, 1);
  res = resolvenewstructindex(fs, proto, slotname, &s);
  if (res == STRUCT_INDEX_SLOT) {
    res = luaK_checkslotassignment(fs, slotname, e, &s.t);
    if (s.nsteps == 1) {
      lua_assert(s.steps[0].s != NULL);
      luaK_setslot(fs, s.steps[0].s, &s.t, var->u.s.info, NO_REG, reg, res);
    }
    else {  /* s.nsteps != 1 */
      int b;
      int type = s.t.type;
      if (type == LUA_TNONE) type = LUA_TNIL;
      b = s.nsteps/3;
      lua_assert((b & SLOTMT_TAG_CHAIN_MASK) == b);
      b |= (type << SLOTMT_POS_TYPE);
      luaK_codeABC(fs, OP_SETSLOTMT, var->u.s.info, b, reg);
      if (type == LUA_TSTRUCT) {
        lua_assert(s.t.proto != NULL);
        luaK_codeABx(fs, OP_DATA, 0, s.t.proto->id);
      }
      codetagchain(fs, &s);
    }
    return 1;
  }
  else if (res == STRUCT_INDEX_INVALID)
    cannot_resolve(fs, proto, slotname);
  return 0;
}

#endif /* HKSC_STRUCTURE_EXTENSION_ON */


void luaK_dischargevars (FuncState *fs, expdesc *e) {
  switch (e->k) {
    case VLOCAL: {
      e->k = VNONRELOC;
      break;
    }
    case VUPVAL: {
      e->u.s.info = luaK_codeABC(fs, OP_GETUPVAL, 0, e->u.s.info, 0);
      e->k = VRELOCABLE;
      break;
    }
    case VGLOBAL: {
#if HKSC_GETGLOBAL_MEMOIZATION
      int k = e->u.s.info;
      if (Settings(fs->H).skip_memo == 0) {
        OpCode op =Settings(fs->H).emit_memo ? OP_GETGLOBAL_MEM : OP_GETGLOBAL;
        e->u.s.info = luaK_codeABx(fs, op, 0, k);
        luaK_codeABx(fs, OP_DATA, 20, addmemoslot(fs, k));
      }
      else
#endif /* HKSC_GETGLOBAL_MEMOIZATION */
      e->u.s.info = luaK_codeABx(fs, OP_GETGLOBAL, 0, e->u.s.info);
      e->k = VRELOCABLE;
      break;
    }
#if HKSC_STRUCTURE_EXTENSION_ON
    case VSLOT: {
      const TValue *index = &e->u.s.structindex;
      freereg(fs, e->u.s.aux);
      freereg(fs, e->u.s.info);
      if (ttisstring(index)) {
        int pc = code_getslot(fs, e, e->u.s.proto, rawtsvalue(index), NO_REG);
        if (pc != -1) {
          e->u.s.info = pc;
          e->k = VRELOCABLE;
          break;
        }
      }
      e->u.s.info = luaK_codeABC(fs, OP_GETTABLE, 0, e->u.s.info, e->u.s.aux);
      e->k = VRELOCABLE;
      luaK_codeABx(fs, OP_DATA, 0, 0);
      luaK_codeABx(fs, OP_DATA, 0, 0);
      break;
    }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    case VINDEXED: {
      freereg(fs, e->u.s.aux);
      freereg(fs, e->u.s.info);
      e->u.s.info = luaK_codeABC(fs, OP_GETTABLE, 0, e->u.s.info, e->u.s.aux);
      e->k = VRELOCABLE;
      luaK_codeABx(fs, OP_DATA, 0, 0);
      luaK_codeABx(fs, OP_DATA, 0, 0);
      break;
    }
    case VVARARG:
    case VCALL: {
      luaK_setoneret(fs, e);
      break;
    }
    default: break;  /* there is one value available (somewhere) */
  }
}


static int code_label (FuncState *fs, int A, int b, int jump) {
  luaK_getlabel(fs);  /* those instructions may be jump targets */
  return luaK_codeABC(fs, OP_LOADBOOL, A, b, jump);
}


static void discharge2reg (FuncState *fs, expdesc *e, int reg) {
  luaK_dischargevars(fs, e);
  switch (e->k) {
    case VNIL: {
      luaK_nil(fs, reg, 1);
      break;
    }
    case VFALSE:  case VTRUE: {
      luaK_codeABC(fs, OP_LOADBOOL, reg, e->k == VTRUE, 0);
      break;
    }
    case VK: {
      luaK_codeABx(fs, OP_LOADK, reg, e->u.s.info);
      break;
    }
    case VKNUM: {
      luaK_codeABx(fs, OP_LOADK, reg, luaK_numberK(fs, e->u.nval));
      break;
    }
    case VRELOCABLE: {
      Instruction *pc = &getcode(fs, e);
      SETARG_A(*pc, reg);
      break;
    }
    case VNONRELOC: {
      if (reg != e->u.s.info)
        luaK_codeABC(fs, OP_MOVE, reg, e->u.s.info, 0);
      break;
    }
    default: {
      lua_assert(e->k == VVOID || e->k == VJMP);
      return;  /* nothing to do... */
    }
  }
  e->u.s.info = reg;
  e->k = VNONRELOC;
}


static void discharge2anyreg (FuncState *fs, expdesc *e) {
  if (e->k != VNONRELOC) {
    luaK_reserveregs(fs, 1);
    discharge2reg(fs, e, fs->freereg-1);
  }
}


static void exp2reg (FuncState *fs, expdesc *e, int reg) {
  discharge2reg(fs, e, reg);
  if (e->k == VJMP)
    luaK_concat(fs, &e->t, e->u.s.info);  /* put this jump in `t' list */
  if (hasjumps(e)) {
    int final;  /* position after whole expression */
    int p_f = NO_JUMP;  /* position of an eventual LOAD false */
    int p_t = NO_JUMP;  /* position of an eventual LOAD true */
    if (need_value(fs, e->t) || need_value(fs, e->f)) {
      int fj = (e->k == VJMP) ? NO_JUMP : luaK_jump(fs);
      p_f = code_label(fs, reg, 0, 1);
      p_t = code_label(fs, reg, 1, 0);
      luaK_patchtohere(fs, fj);
    }
    final = luaK_getlabel(fs);
    patchlistaux(fs, e->f, final, reg, p_f);
    patchlistaux(fs, e->t, final, reg, p_t);
  }
  e->f = e->t = NO_JUMP;
  e->u.s.info = reg;
  e->k = VNONRELOC;
}


void luaK_exp2nextreg (FuncState *fs, expdesc *e) {
  luaK_dischargevars(fs, e);
  freeexp(fs, e);
  luaK_reserveregs(fs, 1);
  exp2reg(fs, e, fs->freereg - 1);
}


int luaK_exp2anyreg (FuncState *fs, expdesc *e) {
  luaK_dischargevars(fs, e);
  if (e->k == VNONRELOC) {
    if (!hasjumps(e)) return e->u.s.info;  /* exp is already in a register */
    if (e->u.s.info >= fs->nactvar) {  /* reg. is not a local? */
      exp2reg(fs, e, e->u.s.info);  /* put value on it */
      return e->u.s.info;
    }
  }
  luaK_exp2nextreg(fs, e);  /* default */
  return e->u.s.info;
}


void luaK_exp2val (FuncState *fs, expdesc *e) {
  if (hasjumps(e))
    luaK_exp2anyreg(fs, e);
  else
    luaK_dischargevars(fs, e);
}


int luaK_exp2RK (FuncState *fs, expdesc *e) {
  luaK_exp2val(fs, e);
  switch (e->k) {
    case VKNUM:
    case VTRUE:
    case VFALSE:
    case VNIL: {
      if (fs->nk <= MAXINDEXRK) {  /* constant fit in RK operand? */
        e->u.s.info = (e->k == VNIL)  ? nilK(fs) :
                      (e->k == VKNUM) ? luaK_numberK(fs, e->u.nval) :
                                        boolK(fs, (e->k == VTRUE));
        e->k = VK;
        return RKASK(e->u.s.info);
      }
      else break;
    }
    case VK: {
      if (e->u.s.info <= MAXINDEXRK)  /* constant fit in argC? */
        return RKASK(e->u.s.info);
      else break;
    }
    default: break;
  }
  /* not a constant in the right range: put it in a register */
  return luaK_exp2anyreg(fs, e);
}



#if HKSC_STRUCTURE_EXTENSION_ON
void luaK_checktype (FuncState *fs, TypeInfo *t, int reg) {
  if (t->type == LUA_TSTRUCT) {
    lua_assert(t->proto != NULL);
    luaK_codeABx(fs, OP_CHECKTYPES, reg, t->proto->id);
  }
  else if (t->type > LUA_TNIL)
    luaK_codeABx(fs, OP_CHECKTYPE, reg, t->type);
}


static int checkstatictype (FuncState *fs, TypeInfo *t, expdesc *exp) {
  int type = t->type; StructProto *proto = t->proto;
  if (type == LUA_TNIL || exptype(exp) == LUA_TNIL)
    return 0;  /* no type-check needed for nil-variables or nil-values */
  if (exptype(exp) == LUA_TNONE)
    return 1;  /* append a type-check code for EXP */
  /* both types are known; compare them */
  if (exptype(exp) == type && (type != LUA_TSTRUCT || expproto(exp) == proto))
    return 0;  /* already matching types; no type-check code needed */
  else {  /* static type-check failed */
    const char *s1, *s2, *s3, *s4;
    if (type == LUA_TSTRUCT) {
      s1 = "struct ";
      s2 = getstr(proto->name);
    }
    else {
      s1 = "";
      s2 = luaX_typename(type);
    }
    if (exptype(exp) == LUA_TSTRUCT) {
      s3 = "struct ";
      s4 = getstr(expproto(exp)->name);
    }
    else {
      s3 = "";
      s4 = luaX_typename(exptype(exp));
    }
    luaX_semerror(fs->ls, luaO_pushfstring(fs->H, "Attempting to set local "
                    "variable of type '%s%s' to type '%s%s'", s1, s2, s3, s4));
  }
  return 0;
}


void luaK_applytypecontraint (FuncState *fs, TypeInfo *t, expdesc *e) {
  if (t->type != LUA_TNONE && checkstatictype(fs, t, e))
    luaK_checktype(fs, t, e->u.s.info);
}


int luaK_checkslotassignment (FuncState *fs, TString *slotname, expdesc *e,
                                const TypeInfo *t) {
  int type;
  StructProto *proto = NULL;
  if (t->is_static == 0)
    return ASSIGN_SLOT_DYNAMIC;
  switch (e->k) {
    case VNIL:
      type = LUA_TNIL;
      break;
    case VTRUE:
    case VFALSE:
      type = LUA_TBOOLEAN;
      break;
    case VK:
      type = ttype(&fs->f->k[e->u.s.info]);
      break;
    case VKNUM:
      type = LUA_TNUMBER;
      break;
    default:
      type = exptype(e);
      proto = expproto(e);
      break;
  }
  if (type == LUA_TNIL)
    return ASSIGN_SLOT_NIL;
  if (t->type != LUA_TNONE) {
    if (type == LUA_TNONE)
      return ASSIGN_SLOT_DYNAMIC;
    if (t->type != type) {  /* static type mismatch */
      luaX_semerror(fs->ls, luaO_pushfstring(fs->H, "Attempt to assign a "
        "value of invalid type to slot '%s' (expected '%s').",getstr(slotname),
        luaX_typename(t->type)));
    }
    else if (type == LUA_TSTRUCT && t->proto != proto) {
      luaX_semerror(fs->ls, luaO_pushfstring(fs->H, "Attempt to assign an "
        "instance of '%s' to slot '%s' (expected '%s').", getstr(proto->name),
        getstr(slotname), getstr(t->proto->name)));
    }
  }
  return ASSIGN_SLOT_STATIC;
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


void luaK_storevar (FuncState *fs, expdesc *var, expdesc *ex) {
  switch (var->k) {
    case VLOCAL: {
      TypeInfo *t;
      freeexp(fs, ex);
      exp2reg(fs, ex, var->u.s.info);
#if HKSC_STRUCTURE_EXTENSION_ON
      /* append a type-check code for the local variable if needed */
      t = &fs->a->locvarstyping[var->u.s.info];
      if (t->is_static && checkstatictype(fs, t, ex))
        luaK_checktype(fs, t, var->u.s.info);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
      UNUSED(t);
      return;
    }
    case VUPVAL: {
      int e = luaK_exp2anyreg(fs, ex);
#if HKSC_STRUCTURE_EXTENSION_ON
      if (exptype(var) != LUA_TNONE) {  /* upvalue is typed */
        TypeInfo t; t.type = exptype(var); t.proto = expproto(var);
        if (checkstatictype(fs, &t, ex))
          luaK_checktype(fs, &t, ex->u.s.info);
      }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
      luaK_codeABC(fs, OP_SETUPVAL, e, var->u.s.info, 0);
      break;
    }
    case VGLOBAL: {
      int e = luaK_exp2anyreg(fs, ex);
      luaK_codeABx(fs, OP_SETGLOBAL, e, var->u.s.info);
      break;
    }
#if HKSC_STRUCTURE_EXTENSION_ON
    case VSLOT: {
      int e = luaK_exp2RK(fs, ex);
      if (!assignslot(fs, var, ex, e))
        luaK_codeABC(fs, OP_SETTABLE, var->u.s.info, var->u.s.aux, e);
      break;
    }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    case VINDEXED: {
      int e = luaK_exp2RK(fs, ex);
      luaK_codeABC(fs, OP_SETTABLE, var->u.s.info, var->u.s.aux, e);
      break;
    }
    default: {
      lua_assert(0);  /* invalid var kind to store */
      break;
    }
  }
  freeexp(fs, ex);
}


#ifdef LUA_CODIW6
void luaK_delete (FuncState *fs, expdesc *var) {
  switch (var->k) {
    case VLOCAL: {
      luaK_codeABC(fs, OP_DELETE, var->u.s.info, 0, DELETE_LOCAL);
      break;
    }
    case VUPVAL: {
      luaK_codeABC(fs, OP_DELETE, 0, var->u.s.info, DELETE_UPVAL);
      break;
    }
    case VGLOBAL: {
      luaK_codeABC(fs, OP_DELETE_BK, 0, var->u.s.info, DELETE_GLOBAL);
      break;
    }
    case VINDEXED: {
      luaK_codeABC(fs, OP_DELETE, var->u.s.info, var->u.s.aux, DELETE_INDEXED);
      break;
    }
    case VSLOT: {
      /* VSLOT is accepted but no code is generated */
      break;
    }
    default: {
      lua_assert(0);  /* invalid var kind to delete */
      break;
    }
  }
}
#endif /* LUA_CODIW6 */


#if HKSC_STRUCTURE_EXTENSION_ON
static int codeselfslot (FuncState *fs, expdesc *e, int k, int reg) {
  const TValue *index;
  if (!expisstruct(e) || !ISK(k))
    return 0;
  index = &fs->f->k[INDEXK(k)];
  if (!ttisstring(index))
    return 0;
  return (code_getslot(fs, e, expproto(e), rawtsvalue(index), reg) != -1);
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


void luaK_self (FuncState *fs, expdesc *e, expdesc *key) {
  int func, c;
  luaK_exp2anyreg(fs, e);
  freeexp(fs, e);
  func = fs->freereg;
  luaK_reserveregs(fs, 2);
  c = luaK_exp2RK(fs, key);
#if HKSC_STRUCTURE_EXTENSION_ON
  if (!codeselfslot(fs, e, c, func))
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  luaK_codeABC(fs, OP_SELF, func, e->u.s.info, c);
  freeexp(fs, key);
  e->u.s.info = func;
  e->k = VNONRELOC;
}


static void invertjump (FuncState *fs, expdesc *e) {
  Instruction *pc = getjumpcontrol(fs, e->u.s.info);
  lua_assert(testTMode(GET_OPCODE(*pc)) && GET_OPCODE(*pc) != OP_TESTSET &&
                                           GET_OPCODE(*pc) != OP_TEST);
  SETARG_A(*pc, !(GETARG_A(*pc)));
}


static int jumponcond (FuncState *fs, expdesc *e, int cond) {
  if (e->k == VRELOCABLE) {
    Instruction ie = getcode(fs, e);
    if (GET_OPCODE(ie) == OP_NOT) {
      fs->pc--;  /* remove previous OP_NOT */
      return condjump(fs, OP_TEST, GETARG_B(ie), 0, !cond);
    }
    /* else go through */
  }
  discharge2anyreg(fs, e);
  freeexp(fs, e);
  return condjump(fs, OP_TESTSET, NO_REG, e->u.s.info, cond);
}


void luaK_goiftrue (FuncState *fs, expdesc *e) {
  int pc;  /* pc of last jump */
  luaK_dischargevars(fs, e);
  switch (e->k) {
    case VK: case VKNUM: case VTRUE: {
      pc = NO_JUMP;  /* always true; do nothing */
      break;
    }
    case VFALSE: {
      pc = luaK_jump(fs);  /* always jump */
      break;
    }
    case VJMP: {
      invertjump(fs, e);
      pc = e->u.s.info;
      break;
    }
    default: {
      pc = jumponcond(fs, e, 0);
      break;
    }
  }
  luaK_concat(fs, &e->f, pc);  /* insert last jump in `f' list */
  luaK_patchtohere(fs, e->t);
  e->t = NO_JUMP;
}


static void luaK_goiffalse (FuncState *fs, expdesc *e) {
  int pc;  /* pc of last jump */
  luaK_dischargevars(fs, e);
  switch (e->k) {
    case VNIL: case VFALSE: {
      pc = NO_JUMP;  /* always false; do nothing */
      break;
    }
    case VTRUE: {
      pc = luaK_jump(fs);  /* always jump */
      break;
    }
    case VJMP: {
      pc = e->u.s.info;
      break;
    }
    default: {
      pc = jumponcond(fs, e, 1);
      break;
    }
  }
  luaK_concat(fs, &e->t, pc);  /* insert last jump in `t' list */
  luaK_patchtohere(fs, e->f);
  e->f = NO_JUMP;
}


static void codenot (FuncState *fs, expdesc *e) {
  luaK_dischargevars(fs, e);
  switch (e->k) {
    case VNIL: case VFALSE: {
      e->k = VTRUE;
      exptype(e) = LUA_TBOOLEAN; expproto(e) = NULL;
      break;
    }
    case VK: case VKNUM: case VTRUE: {
      e->k = VFALSE;
      exptype(e) = LUA_TBOOLEAN; expproto(e) = NULL;
      break;
    }
    case VJMP: {
      invertjump(fs, e);
      break;
    }
    case VRELOCABLE:
    case VNONRELOC: {
      discharge2anyreg(fs, e);
      freeexp(fs, e);
      e->u.s.info = luaK_codeABC(fs, OP_NOT, 0, e->u.s.info, 0);
      e->k = VRELOCABLE;
      break;
    }
    default: {
      lua_assert(0);  /* cannot happen */
      break;
    }
  }
  /* interchange true and false lists */
  { int temp = e->f; e->f = e->t; e->t = temp; }
  removevalues(fs, e->f);
  removevalues(fs, e->t);
}


void luaK_indexed (FuncState *fs, expdesc *t, expdesc *k) {
  t->u.s.aux = luaK_exp2RK(fs, k);
  t->k = VINDEXED;
#if HKSC_STRUCTURE_EXTENSION_ON
  if (Settings(fs->H).emit_struct) {
    if (exptype(t) == LUA_TSTRUCT) {
      lua_assert(expproto(t) != NULL);
      switch (k->k) {
        case VK: {
          const TValue *o = &fs->f->k[k->u.s.info];
          t->u.s.proto = expproto(t);
          setobj(&t->u.s.structindex, o);
          t->k = VSLOT;
          if (ttisstring(o)) {
            int result;
            StructSlotLookup s;
            init_struct_lookup(fs, &s, 0);
            result = resolvestructindex(fs, expproto(t), rawtsvalue(o), &s);
            if (result == STRUCT_INDEX_SLOT && s.t.is_static) {
              exptype(t) = s.t.type;
              expproto(t) = s.t.proto;
              return;
            }
          }
          break;
        }
        case VTRUE:
        case VFALSE: {
          t->u.s.proto = expproto(t);
          setbvalue(&t->u.s.structindex, k->k == VTRUE);
          t->k = VSLOT;
          break;
        }
        case VKNUM: {
          t->u.s.proto = expproto(t);
          setnvalue(&t->u.s.structindex, k->u.nval);
          t->k = VSLOT;
          break;
        }
        default: break;
      }
    }
    exptype(t) = LUA_TNONE;
    expproto(t) = NULL;
  }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
}


static int constfolding (OpCode op, expdesc *e1, expdesc *e2) {
  lua_Number v1, v2, r;
#ifdef LUA_CODT7
  lu_int32 iv1, iv2;
#endif /* LUA_CODT7 */
  if (!isnumeral(e1) || !isnumeral(e2)) return 0;
  v1 = e1->u.nval;
  v2 = e2->u.nval;
#ifdef LUA_CODT7
  iv1 = cast(lu_int32, v1);
  iv2 = cast(lu_int32, v2);
#endif /* LUA_CODT7 */
  switch (op) {
    case OP_ADD: r = luai_numadd(v1, v2); break;
    case OP_SUB: r = luai_numsub(v1, v2); break;
    case OP_MUL: r = luai_nummul(v1, v2); break;
    case OP_DIV:
      if (v2 == 0) return 0;  /* do not attempt to divide by 0 */
      r = luai_numdiv(v1, v2); break;
    case OP_MOD:
      if (v2 == 0) return 0;  /* do not attempt to divide by 0 */
      r = luai_nummod(v1, v2); break;
    case OP_POW: r = luai_numpow(v1, v2); break;
    case OP_UNM: r = luai_numunm(v1); break;
    case OP_LEN: return 0;  /* no constant folding for 'len' */
#ifdef LUA_CODT7
    case OP_LEFT_SHIFT: r = cast_num(iv1 << iv2); break;
    case OP_RIGHT_SHIFT: r = cast_num(iv1 >> iv2); break;
    case OP_BIT_AND: r = cast_num(iv1 & iv2); break;
    case OP_BIT_OR: r = cast_num(iv1 | iv2); break;
#endif /* LUA_CODT7 */
    default: lua_assert(0); r = 0; break;
  }
  if (luai_numisnan(r)) return 0;  /* do not attempt to produce NaN */
  e1->u.nval = r;
  return 1;
}

static void codearith (FuncState *fs, OpCode op, expdesc *e1, expdesc *e2) {
  if (constfolding(op, e1, e2))
    return;
  else {
    int o2 = (op != OP_UNM && op != OP_LEN) ? luaK_exp2RK(fs, e2) : 0;
    int o1 = luaK_exp2RK(fs, e1);
    if (o1 > o2) {
      freeexp(fs, e1);
      freeexp(fs, e2);
    }
    else {
      freeexp(fs, e2);
      freeexp(fs, e1);
    }
    e1->u.s.info = luaK_codeABC(fs, op, 0, o1, o2);
    e1->k = VRELOCABLE;
    switch (op) {
      case OP_ADD:
      case OP_SUB:
      case OP_MUL:
      case OP_DIV:
      case OP_MOD:
      case OP_POW:
      case OP_UNM:
#ifdef LUA_CODT7
      case OP_LEFT_SHIFT:
      case OP_RIGHT_SHIFT:
      case OP_BIT_AND:
      case OP_BIT_OR:
#endif /* LUA_CODT7 */
      {
        if (!expisnumber(e1) || !expisnumber(e2))
          exptype(e1) = LUA_TNONE;
        break;
      }
      case OP_LEN: {
        if (expisstring(e1) || expistable(e1) || expisstruct(e1))
          exptype(e1) = LUA_TNUMBER;
        else
          exptype(e1) = LUA_TNONE;
        break;
      }
      case OP_CONCAT: {
        if ((expisnumber(e1) || expisstring(e1)) &&
            (expisnumber(e2) || expisstring(e2)))
          exptype(e1) = LUA_TSTRING;
        else
          exptype(e1) = LUA_TNONE;
        break;
      }
      default: break;
    }
  }
}


static void codecomp (FuncState *fs, OpCode op, int cond, expdesc *e1,
                                                          expdesc *e2) {
  int o1 = luaK_exp2RK(fs, e1);
  int o2 = luaK_exp2RK(fs, e2);
  freeexp(fs, e2);
  freeexp(fs, e1);
  if (cond == 0 && op != OP_EQ) {
    int temp;  /* exchange args to replace by `<' or `<=' */
    temp = o1; o1 = o2; o2 = temp;  /* o1 <==> o2 */
    cond = 1;
  }
  e1->u.s.info = condjump(fs, op, cond, o1, o2);
  e1->k = VJMP;
  exptype(e1) = LUA_TBOOLEAN;
  expproto(e1) = NULL;
}


void luaK_prefix (FuncState *fs, UnOpr op, expdesc *e) {
  expdesc e2;
  e2.t = e2.f = NO_JUMP; e2.k = VKNUM; e2.u.nval = 0;
  switch (op) {
    case OPR_MINUS: {
      if (!isnumeral(e)) {
        luaK_exp2anyreg(fs, e);  /* cannot operate on non-numeric constants */
        exptype(e) = LUA_TNONE;
      }
      codearith(fs, OP_UNM, e, &e2);
      break;
    }
    case OPR_NOT: codenot(fs, e); break;
    case OPR_LEN: {
      luaK_exp2anyreg(fs, e);  /* cannot operate on constants */
      codearith(fs, OP_LEN, e, &e2);
      break;
    }
    default: lua_assert(0);
  }
}


void luaK_infix (FuncState *fs, BinOpr op, expdesc *v) {
  switch (op) {
    case OPR_AND: {
      luaK_goiftrue(fs, v);
      break;
    }
    case OPR_OR: {
      luaK_goiffalse(fs, v);
      break;
    }
    case OPR_CONCAT: {
      luaK_exp2nextreg(fs, v);  /* operand must be on the `stack' */
      break;
    }
    case OPR_ADD: case OPR_SUB: case OPR_MUL: case OPR_DIV:
    case OPR_MOD: case OPR_POW:
#ifdef LUA_CODT7
    case OPR_LEFT_SHIFT: case OPR_RIGHT_SHIFT:
    case OPR_BIT_AND: case OPR_BIT_OR:
#endif /* LUA_CODT7 */
    {
      if (!isnumeral(v)) {
        luaK_exp2RK(fs, v);
        if (exptype(v) != LUA_TNUMBER)
          exptype(v) = LUA_TNONE;
      }
      break;
    }
    default: {
      luaK_exp2RK(fs, v);
      exptype(v) = LUA_TNONE;
      break;
    }
  }
}


void luaK_posfix (FuncState *fs, BinOpr op, expdesc *e1, expdesc *e2) {
  switch (op) {
    case OPR_AND: {
      lua_assert(e1->t == NO_JUMP);  /* list must be closed */
      luaK_dischargevars(fs, e2);
      luaK_concat(fs, &e2->f, e1->f);
      if (!ismatchingtype(e1, e2)) {
        exptype(e2) = LUA_TNONE; expproto(e2) = NULL;
      }
      *e1 = *e2;
      break;
    }
    case OPR_OR: {
      lua_assert(e1->f == NO_JUMP);  /* list must be closed */
      luaK_dischargevars(fs, e2);
      luaK_concat(fs, &e2->t, e1->t);
      if (!ismatchingtype(e1, e2)) {
        exptype(e2) = LUA_TNONE; expproto(e2) = NULL;
      }
      *e1 = *e2;
      break;
    }
    case OPR_CONCAT: {
      Instruction *pi = (luaK_exp2val(fs, e2), &getcode(fs, e2));
      if (e2->k == VRELOCABLE && GET_OPCODE(*pi) == OP_CONCAT) {
        lua_assert(e1->u.s.info == GETARG_B(*pi)-1);
        freeexp(fs, e1);
        SETARG_B(*pi, e1->u.s.info);
        e1->k = VRELOCABLE; e1->u.s.info = e2->u.s.info;
      }
      else {
        luaK_exp2nextreg(fs, e2);  /* operand must be on the 'stack' */
        codearith(fs, OP_CONCAT, e1, e2);
      }
      if (exptype(e1) != LUA_TSTRING || exptype(e2) != LUA_TSTRING)
        exptype(e1) = LUA_TNONE;
      break;
    }
    case OPR_ADD: codearith(fs, OP_ADD, e1, e2); break;
    case OPR_SUB: codearith(fs, OP_SUB, e1, e2); break;
    case OPR_MUL: codearith(fs, OP_MUL, e1, e2); break;
    case OPR_DIV: codearith(fs, OP_DIV, e1, e2); break;
    case OPR_MOD: codearith(fs, OP_MOD, e1, e2); break;
    case OPR_POW: codearith(fs, OP_POW, e1, e2); break;
#ifdef LUA_CODT7 /* T7 extensions */
    case OPR_LEFT_SHIFT: codearith(fs, OP_LEFT_SHIFT, e1, e2); break;
    case OPR_RIGHT_SHIFT: codearith(fs, OP_RIGHT_SHIFT, e1, e2); break;
    case OPR_BIT_AND: codearith(fs, OP_BIT_AND, e1, e2); break;
    case OPR_BIT_OR: codearith(fs, OP_BIT_OR, e1, e2); break;
#endif /* LUA_CODT7 */
    case OPR_EQ: codecomp(fs, OP_EQ, 1, e1, e2); break;
    case OPR_NE: codecomp(fs, OP_EQ, 0, e1, e2); break;
    case OPR_LT: codecomp(fs, OP_LT, 1, e1, e2); break;
    case OPR_LE: codecomp(fs, OP_LE, 1, e1, e2); break;
    case OPR_GT: codecomp(fs, OP_LT, 0, e1, e2); break;
    case OPR_GE: codecomp(fs, OP_LE, 0, e1, e2); break;
    default: lua_assert(0);
  }
}


void luaK_fixline (FuncState *fs, int line) {
  fs->f->lineinfo[fs->pc - 1] = line;
}


static int luaK_code (FuncState *fs, Instruction i, int line) {
  Proto *f = fs->f;
  dischargejpc(fs);  /* `pc' will change */
  /* put new instruction in code array */
  luaM_growvector(fs->H, f->code, fs->pc, f->sizecode, Instruction,
                  MAX_INT, "code size overflow");
  f->code[fs->pc] = i;
  /* save corresponding line information */
  luaM_growvector(fs->H, f->lineinfo, fs->pc, f->sizelineinfo, int,
                  MAX_INT, "code size overflow");
  f->lineinfo[fs->pc] = line;
  return fs->pc++;
}


int luaK_codeABC (FuncState *fs, OpCode o, int a, int b, int c) {
  lua_assert(getOpMode(o) == iABC);
  lua_assert(getBMode(o) != OpArgN || b == 0);
  lua_assert(getCMode(o) != OpArgN || c == 0);
  return luaK_code(fs, CREATE_ABC(o, a, b, c), fs->ls->lastline);
}


int luaK_codeABx (FuncState *fs, OpCode o, int a, unsigned int bc) {
  lua_assert(getOpMode(o) == iABx || getOpMode(o) == iAsBx);
  lua_assert(getCMode(o) == OpArgN);
  return luaK_code(fs, CREATE_ABx(o, a, bc), fs->ls->lastline);
}


void luaK_setlist (FuncState *fs, int base, int nelems, int tostore) {
  int c =  (nelems - 1)/LFIELDS_PER_FLUSH + 1;
  int b = (tostore == LUA_MULTRET) ? 0 : tostore;
  lua_assert(tostore != 0);
  if (c <= MAXARG_C)
    luaK_codeABC(fs, OP_SETLIST, base, b, c);
  else {
    if (c > MAXARG_Bx)
      luaX_inputerror(fs->ls, "Attempt to initialize a table with too many "
        "array literals. Please split into multiple statements.");
    luaK_codeABC(fs, OP_SETLIST, base, b, 0);
    luaK_codeABx(fs, OP_DATA, 0, c);
  }
  fs->freereg = base + 1;  /* free registers with list values */
}


/******************************************************************************
** Code optimization **********************************************************
******************************************************************************/

/*
** Identify `leaders' in an instruction sequence
*/
static void identify_leaders (int sizecode, Instruction *code,
                              lu_byte *properties) {
  int i;
  memset(properties, 0, sizecode);
  for (i = 0; i < sizecode; i++) {
    Instruction instr = code[i];
    OpCode op = GET_OPCODE(instr);
    if (testTMode(op) || (op == OP_LOADBOOL && GETARG_C(instr) != 0)) {
      lua_assert(i < (sizecode - 2));
      properties[i+1] = 1; /* jump instruction is leader */
      properties[i+2] = 1; /* true-target is leader */
    } else if (op == OP_JMP || op == OP_FORLOOP || op == OP_FORPREP) {
      int offs = GETARG_sBx(instr);
      lua_assert(0 <= i+1+offs && i+1+offs < sizecode);
      properties[i+1] = 1; /* next instruction is leader */
      properties[i+1+offs] = 1;/* jump target (maybe false-target) is leader */
    }
  }
}


/*
** Optimize the code sequence of a function
*/
static void specialize_instruction_sequence (hksc_State *H, Instruction *code,
                                             int sizecode, const TValue *k) {
  int i;
  lu_byte *properties;
  for (i = 0; i < sizecode; i++) {
    OpCode op = GET_OPCODE(code[i]);
    switch (op) {
      case OP_GETTABLE: { /* change to GETTABLE_S or GETFIELD */
        int c = GETARG_C(code[i]);
        if (!ISK(c) || !ttisstring(&k[INDEXK(c)]))
          SET_OPCODE(code[i], OP_GETTABLE_S);
        else {
          SET_OPCODE(code[i], OP_GETFIELD);
          SETARG_C(code[i], INDEXK(c));
        }
        break;
      }
      case OP_SETTABLE:
      case OP_SETTABLE_BK: { /* change to SETTABLE_S or SETFIELD */
        int b = GETARG_B(code[i]);
        if (!ISK(b) || !ttisstring(&k[INDEXK(b)]))
          SET_OPCODE(code[i], OP_SETTABLE_S);
        else
          /* don't use SET_OPCODE because the BK bit needs to be cleared */
          code[i] = CREATE_ABC(OP_SETFIELD, GETARG_A(code[i]), INDEXK(b),
                               GETARG_C(code[i]));
        break;
      }
      case OP_CALL: /* change to CALL_I */
        SET_OPCODE(code[i], OP_CALL_I);
        break;
      case OP_TAILCALL: /* change to TAILCALL_I */
        SET_OPCODE(code[i], OP_TAILCALL_I);
        break;
      default: break;
    }
  }
  properties = luaM_newvector(H, sizecode, lu_byte);
  identify_leaders(sizecode, code, properties);
  for (i = 1; i < sizecode; i++) {
    if (properties[i] == 0) { /* current instruction is not a leader */
      int prevIndex = i-1;
      Instruction prev, curr;
      OpCode prevOpCode, currOpCode;
      prev = code[prevIndex];
      curr = code[i];
      while (GET_OPCODE(prev) == OP_DATA && prevIndex > 0)
        prev = code[--prevIndex];
      prevOpCode = GET_OPCODE(prev);
      currOpCode = GET_OPCODE(curr);
      if (testMakeR1(prevOpCode)) {
        OpCode r1Version = getR1Version(currOpCode);
        if (r1Version != OP_MAX &&
          ((getR1Mode(r1Version) == R1A && GETARG_A(prev) == GETARG_A(curr)) ||
           (getR1Mode(r1Version) == R1B && GETARG_A(prev) == GETARG_B(curr))))
          SET_OPCODE(code[i], r1Version); /* set to R1 version */
      }
    }
  }
  luaM_freearray(H, properties, sizecode, lu_byte);
}


void luaK_optimize_function (hksc_State *H, Proto *f) {
  int i,n;
  specialize_instruction_sequence(H, f->code, f->sizecode, f->k);
  n=f->sizep;
  for (i=0; i<n; i++) luaK_optimize_function(H, f->p[i]);
}

