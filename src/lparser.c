/*
** $Id: lparser.c $
** Lua Parser
** See Copyright Notice in lua.h
*/


#include <string.h>

#define lparser_c
#define LUA_CORE

#include "hksclua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "lstruct.h"
#include "ltable.h"



#define hasmultret(k)		((k) == VCALL || (k) == VVARARG)

#define getlocvar(fs, i)	((fs)->f->locvars[(fs)->actvar[i]])

#define luaY_checklimit(fs,v,l,m)	if ((v)>(l)) errorlimit(fs,l,m)


/*
** nodes for block list (list of active blocks)
*/
typedef struct BlockCnt {
  struct BlockCnt *previous;  /* chain */
  int breaklist;  /* list of jumps out of this loop */
  lu_byte nactvar;  /* # active locals outside the breakable structure */
  lu_byte upval;  /* true if some variable in the block is an upvalue */
  lu_byte isbreakable;  /* true if `block' is a loop */
} BlockCnt;



/*
** prototypes for recursive non-terminal functions
*/
static void chunk (LexState *ls);
static void expr (LexState *ls, expdesc *v);


static void anchor_token (LexState *ls) {
  if (ls->t.token == TK_NAME || ls->t.token == TK_STRING) {
    TString *ts = ls->t.seminfo.ts;
    luaX_newstring(ls, getstr(ts), ts->tsv.len);
  }
}


static void error_expected (LexState *ls, int token) {
  luaX_syntaxerror(ls,
      luaO_pushfstring(ls->H, "%s expected", luaX_token2str(ls, token)));
}


static void errorlimit (FuncState *fs, int limit, const char *what) {
  const char *msg = (fs->f->linedefined == 0) ?
    luaO_pushfstring(fs->H, "Main function has more than %d %s", limit, what) :
    luaO_pushfstring(fs->H, "Function at line %d has more than %d %s",
                            fs->f->linedefined, limit, what);
  luaX_inputerror(fs->ls, msg);
}


static int testnext (LexState *ls, int c) {
  if (ls->t.token == c) {
    luaX_next(ls);
    return 1;
  }
  else return 0;
}


static void check (LexState *ls, int c) {
  if (ls->t.token != c)
    error_expected(ls, c);
}

static void checknext (LexState *ls, int c) {
  check(ls, c);
  luaX_next(ls);
}


#define check_condition(ls,c,msg)	{ if (!(c)) luaX_syntaxerror(ls, msg); }



static void check_match (LexState *ls, int what, int who, int where) {
  if (!testnext(ls, what)) {
    if (where == ls->linenumber)
      error_expected(ls, what);
    else {
      luaX_syntaxerror(ls, luaO_pushfstring(ls->H,
             "%s expected (to close %s at line %d)",
              luaX_token2str(ls, what), luaX_token2str(ls, who), where));
    }
  }
}


static TString *str_peekname (LexState *ls) {
  TString *ts;
  check(ls, TK_NAME);
  ts = ls->t.seminfo.ts;
  return ts;
}


static TString *str_checkname (LexState *ls) {
  TString *ts;
  check(ls, TK_NAME);
  ts = ls->t.seminfo.ts;
  luaX_next(ls);
  return ts;
}


#define init_exp(e,k,i) init_typed_exp(e,k,i,LUA_TNONE)

static void init_typed_exp (expdesc *e, expkind k, int i, int type) {
  e->f = e->t = NO_JUMP;
  e->k = k;
  e->u.s.info = i;
  e->inferred_type = type;
  e->inferred_proto = NULL;
}


static void setexptypeinfo (expdesc *e, TypeInfo *t) {
  e->inferred_type = t->type;
  e->inferred_proto = t->proto;
}


static void codeliteral (LexState *ls, expdesc *e, lu_int64 l, int token) {
  int type = (token == TK_LITERALLUD) ? LUA_TLIGHTUSERDATA : LUA_TUI64;
  init_typed_exp(e, VK, luaK_literalK(ls->fs, l, token), type);
}


static void codestring (LexState *ls, expdesc *e, TString *s) {
  init_typed_exp(e, VK, luaK_stringK(ls->fs, s), LUA_TSTRING);
}


static void checkname(LexState *ls, expdesc *e) {
  codestring(ls, e, str_checkname(ls));
}


static int registerlocalvar (LexState *ls, TString *varname) {
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  int oldsize = f->sizelocvars;
  luaM_growvector(ls->H, f->locvars, fs->nlocvars, f->sizelocvars,
                  LocVar, SHRT_MAX, "too many local variables");
  while (oldsize < f->sizelocvars) f->locvars[oldsize++].varname = NULL;
  f->locvars[fs->nlocvars].varname = varname;
  /*luaC_objbarrier(ls->L, f, varname);*/
  return fs->nlocvars++;
}


#if !HKSC_STRUCTURE_EXTENSION_ON

#ifdef HKSC_MATCH_HAVOKSCRIPT_ERROR_MSG
#define TYPED_MSG_HELP " See HKS_STRUCTURE_EXTENSION_ON in HksSettings.h."
#else /* !HKSC_MATCH_HAVOKSCRIPT_ERROR_MSG */
#define TYPED_MSG_HELP ""
#endif /* HKSC_MATCH_HAVOKSCRIPT_ERROR_MSG */

#define typed_error_msg(x,d) "Cannot use typed " x " when the virtual " \
"machine is built without structures" d TYPED_MSG_HELP


static void error_typedlocalvar (LexState *ls) {
  luaX_semerror(ls, typed_error_msg("local variables", ""));
}

static void error_typedparam (LexState *ls) {
  luaX_semerror(ls, typed_error_msg("parameters", "."));
}
#endif /* !HKSC_STRUCTURE_EXTENSION_ON */


#define new_localvarliteral(ls,v,n) \
  new_typedlocalvarliteral(ls,v,n,LUA_TNONE,NULL)

#define new_typedlocalvarliteral(ls,v,n,t,p) \
new_typedlocalvar(ls,luaX_newstring(ls, "" v,(sizeof(v)/sizeof(char))-1),n,t,p)

#define new_localvar(ls,name,n)  new_typedlocalvar(ls,name,n,LUA_TNONE,NULL)


static void new_typedlocalvar (LexState *ls, TString *name, int n, int type,
                               struct StructProto *proto) {
  FuncState *fs = ls->fs;
  luaY_checklimit(fs, fs->nactvar+n+1, LUAI_MAXVARS, "local variables");
  fs->actvar[fs->nactvar+n] = cast(unsigned short, registerlocalvar(ls, name));
  /* create space for a new type map entry */
  n = cast_int(fs->nactvar+n);
  luaM_growvector(ls->H, fs->a->locvarstyping, n, fs->a->sizelocvarstyping,
                  TypeInfo, SHRT_MAX, "");
  fs->a->locvarstyping[n].type = type;
  fs->a->locvarstyping[n].proto = proto;
  fs->a->locvarstyping[n].is_static = (type != LUA_TNONE);
}


static void pushlhstyping (LexState *ls, TypeInfo *t) {
  FuncState *fs = ls->fs;
  /* add to the LHS type map */
  luaM_growvector(ls->H, fs->a->lhstyping, fs->nlocalslhs,fs->a->sizelhstyping,
                  TypeInfo, SHRT_MAX, "");
  fs->a->lhstyping[fs->nlocalslhs++] = *t;
}


#if HKSC_STRUCTURE_EXTENSION_ON

static void type_error (LexState *ls, const char *fmt, TString *typename) {
  luaX_semerror(ls, luaO_pushfstring(ls->H, fmt, getstr(typename)));
}


/* return value when the name does not match any builtin type */
#define STUCTURE_TYPE  (LUA_NUM_TYPE_OBJECTS-1)

static int gettypeidfromstring (LexState *ls, TString *ts) {
  int i;
  for (i = 0; i < LUA_NUM_TYPE_OBJECTS+1; i++) {
    if (ts == G(ls->H)->typenames[i])
      break;
  }
  i -= 2;
  return i;
}

/*
** generate type information from a given type name
*/
static void parsetype (LexState *ls, TypeInfo *t, TString *typename) {
  int i = gettypeidfromstring(ls, typename);
  if (i == STUCTURE_TYPE) {  /* structure type */
    StructProto *proto = luaR_getstructbyname(ls->H, typename);
    if (proto == NULL) {
      type_error(ls, "Type declaration '%s' does not match any built-in type "
                 "or structure.", typename);
    }
    t->type = LUA_TSTRUCT;
    t->proto = proto;
    t->is_static = 1;
  }
  else if (i == LUA_TSTRUCT) {  /* "struct" is not allowed */
    luaX_semerror(ls, "Type declaration 'struct' is not allowed.");
  }
  else {  /* built-in type */
    if (i == LUA_TANY) {  /* "object" */
      t->type = LUA_TNONE;
      t->is_static = 0;
    }
    else {
      t->type = i;
      t->is_static = 1;
    }
    t->proto = NULL;
  }
}


static TString *structname (LexState *ls) {
  hksc_State *H = ls->H;
  TString *name = str_checkname(ls);
  if (Settings(H).emit_struct) {
    int i;
    for (i = 0; i < LUA_NUM_TYPE_OBJECTS+1; i++) {
      if (name == G(H)->typenames[i]) {
        type_error(ls, "Cannot use '%s' as a structure name", name);
        break;
      }
    }
  }
  return name;
}


static void initstruct (LexState *ls, TString *name) {
  hksc_State *H = ls->H;
  if (Settings(H).emit_struct) {
    StructProto *p = &ls->current_proto;
    p->nslots = 0;
    p->hasmeta = 0;
    p->hasproxy = 0;
    p->id = LAST_STRUCT_ID;
    p->name = name;
    luaR_addreservedslots(H, &ls->current_proto);
  }
}


static void finalizestruct (LexState *ls) {
  hksc_State *H = ls->H;
  if (Settings(H).emit_struct) {
    StructProto *new_proto = &ls->current_proto;
    StructProto *vm_proto;
    if (new_proto->nslots == NUM_SLOTS_RESERVED &&
        !new_proto->hasmeta && !new_proto->hasproxy)
      luaX_semerror(ls, "Empty structure definitions are not allowed.");
    vm_proto = luaR_getstructbyname(H, new_proto->name);
    if (vm_proto != NULL) {
      if (!luaR_compareproto(new_proto, vm_proto))
      {
        type_error(ls, "Structure '%s' already has a different definition.",
                   new_proto->name);
      }
      return;
    }
    if (G(H)->protolist.nuse + 1 >= LUAI_MAXSTRUCTS) {
      luaX_inputerror(ls, "Cannot define another structure in this VM: too "
                      "many structure prototypes.");
    }
    luaR_addstructproto(H, new_proto);
  }
}


static void addstructproxy (LexState *ls) {
  hksc_State *H = ls->H;
  if (Settings(H).emit_struct) {
    StructProto *p = &ls->current_proto;
    if (p->hasproxy)
      luaX_semerror(ls, "Duplicate 'proxytable' slot definition.");
    p->hasproxy = 1;
  }
}


static void addstructmeta (LexState *ls, TString *typename) {
  hksc_State *H = ls->H;
  if (Settings(H).emit_struct) {
    StructProto *p = &ls->current_proto;
    StructSlot *metaslot;
    int metatype;
    StructProto *metaproto;
    if (p->hasmeta)
      luaX_semerror(ls, "Duplicate 'meta' slot definition.");
    metatype = gettypeidfromstring(ls, typename);
    if (metatype == STUCTURE_TYPE) {
      metatype = LUA_TSTRUCT;
      metaproto = luaR_getstructbyname(H, typename);
      if (metaproto == NULL) {
        if (typename == p->name)
          metaproto = p;
        else {
          type_error(ls, "Undefined structure type '%s' in slot definition.",
                     typename);
        }
      }
    }
    else {
      if (metatype == LUA_TANY) metatype = LUA_TNIL;
      else if (metatype != LUA_TTABLE) {
        type_error(ls, "Invalid type name '%s' for meta slot. Must be a "
                   "object/table/structure name.", typename);
      }
      metaproto = NULL;
    }
    p->hasmeta = 1;
    metaslot = &ls->current_proto.slots[SLOT_INDEX_META];
    metaslot->typeid = metatype;
    metaslot->structid = metaproto ? metaproto->id : 0;
  }
}


static void addstructslot (LexState *ls, TString *slotname, TString *typename){
  hksc_State *H = ls->H;
  if (Settings(H).emit_struct) {
    StructProto *p = &ls->current_proto;
    int type;
    size_t i;
    for (i = 0; i < p->nslots; i++) {
      if (ls->current_proto.slots[i].name == slotname) {
        luaX_semerror(ls, "Duplicate slot name.");
        break;
      }
    }
    if (p->nslots >= MAX_STRUCT_SLOTS) {
      luaX_inputerror(ls, "Too many slots in the structure. Cannot add any "
                      "more.");
    }
    type = gettypeidfromstring(ls, typename);
    if (type == LUA_TSTRUCT) {
      luaX_semerror(ls, "Cannot use 'struct' as a type name. Must be a "
                      "structure name.");
    }
    else if (type == LUA_TUI64) {
      luaX_semerror(ls, "Unable to store UI64 values in a structure.");
    }
    else {
      if (type == STUCTURE_TYPE) {
        StructProto *slotproto = luaR_getstructbyname(H, typename);
        if (slotproto == NULL) {
          /* the slot can contain its parent structure type, which has not yet
             been added to the Lua state */
          if (typename == p->name)
            slotproto = p;
          else {
            type_error(ls, "Undefined structure type '%s' in slot definition.",
                       typename);
          }
        }
        luaR_addslot(H, p, slotname, LUA_TSTRUCT,
                     slotproto->id);
      }
      else {
        if (type == LUA_TANY) type = LUA_TNIL;
        luaR_addslot(H, p, slotname, type, -1);
      }
    }
  }
}


static void typedlocvar (LexState *ls, TString *name, TString *type, int n) {
  TypeInfo t;
  parsetype(ls, &t, type);
  new_typedlocalvar(ls, name, n, t.type, t.proto);
}

#endif /* HKSC_STRUCTURE_EXTENSION_ON */


static void adjustlocalvars (LexState *ls, int nvars) {
  FuncState *fs = ls->fs;
  fs->nactvar = cast_byte(fs->nactvar + nvars);
  for (; nvars; nvars--) {
    getlocvar(fs, fs->nactvar - nvars).startpc = fs->pc;
  }
}


static void removevars (LexState *ls, int tolevel) {
  FuncState *fs = ls->fs;
  while (fs->nactvar > tolevel)
    getlocvar(fs, --fs->nactvar).endpc = fs->pc;
}


static int indexupvalue (FuncState *fs, TString *name, expdesc *v) {
  int i;
  Proto *f = fs->f;
  int oldsize = f->sizeupvalues;
  for (i=0; i<f->nups; i++) {
    if (fs->upvalues[i].k == v->k && fs->upvalues[i].info == v->u.s.info) {
      lua_assert(f->upvalues[i] == name);
      return i;
    }
  }
  /* new one */
  luaY_checklimit(fs, f->nups + 1, LUAI_MAXUPVALUES, "upvalues");
  luaM_growvector(fs->H, f->upvalues, f->nups, f->sizeupvalues,
                  TString *, MAX_INT, "");
  while (oldsize < f->sizeupvalues) f->upvalues[oldsize++] = NULL;
  f->upvalues[f->nups] = name;
  lua_assert(v->k == VLOCAL || v->k == VUPVAL);
  fs->upvalues[f->nups].k = cast_byte(v->k);
  fs->upvalues[f->nups].info = cast_byte(v->u.s.info);
  return f->nups++;
}


static int searchvar (FuncState *fs, TString *n) {
  int i;
  for (i=fs->nactvar-1; i >= 0; i--) {
    if (n == getlocvar(fs, i).varname)
      return i;
  }
  return -1;  /* not found */
}


static void markupval (FuncState *fs, int level) {
  BlockCnt *bl = fs->bl;
  while (bl && bl->nactvar > level) bl = bl->previous;
  if (bl) bl->upval = 1;
}


static int singlevaraux (FuncState *fs, TString *n, expdesc *var, int base) {
  if (fs == NULL) {  /* no more levels? */
    init_exp(var, VGLOBAL, NO_REG);  /* default is global variable */
    return VGLOBAL;
  }
  else {
    int v = searchvar(fs, n);  /* look up at current level */
    if (v >= 0) {
      init_exp(var, VLOCAL, v);
      lua_assert(fs->a->sizelocvarstyping > v);
      setexptypeinfo(var, &fs->a->locvarstyping[v]);
      if (!base)
        markupval(fs, v);  /* local will be used as an upval */
      return VLOCAL;
    }
    else {  /* not found at current level; try upper one */
      if (singlevaraux(fs->prev, n, var, 0) == VGLOBAL)
        return VGLOBAL;
      var->u.s.info = indexupvalue(fs, n, var);  /* else was LOCAL or UPVAL */
      var->k = VUPVAL;  /* upvalue in this level */
      return VUPVAL;
    }
  }
}


static TString *singlevar (LexState *ls, expdesc *var) {
  TString *varname = str_checkname(ls);
  FuncState *fs = ls->fs;
  if (singlevaraux(fs, varname, var, 1) == VGLOBAL)
    var->u.s.info = luaK_stringK(fs, varname); /* info points to global name */
  return varname;
}


/*
** push a new ExpListEntry
*/
static ExpListEntry *pushexplistentry (FuncState *fs) {
  ExpListEntry *l;
  luaM_growvector(fs->H, fs->a->explists, fs->a->nexplists,fs->a->sizeexplists,
                  ExpListEntry, MAX_INT, "");
  l = &fs->a->explists[fs->a->nexplists++];
  l->types = NULL;
  l->constraints = NULL;
  l->hasconstraints = 0;
  l->sizetypes = 0;
  l->sizeconstraints = 0;
  l->ntypes = 0;
  l->nconstraints = 0;
  return l;
}


/*
** return the top ExpListEntry
*/
static ExpListEntry *gettopexplistentry (FuncState *fs) {
  lua_assert(fs->a->nexplists > 0);
  return &fs->a->explists[fs->a->nexplists-1];
}


/*
** pop an ExpListEntry from the stack
*/
static void popexplistentry (FuncState *fs) {
  ExpListEntry *l;
  lua_assert(fs->a->nexplists > 0);
  l = &fs->a->explists[--fs->a->nexplists];
  luaM_freearray(fs->H, l->types, l->sizetypes, TypeInfo);
  luaM_freearray(fs->H, l->constraints, l->sizeconstraints, TypeInfo);
}


/*
** push a new TypeInfo element to the top ExpListEntry's TypeInfo stack
*/
static TypeInfo *pushexplisttypeinfo (FuncState *fs) {
  ExpListEntry *l;
  lua_assert(fs->a->nexplists > 0);
  l = &fs->a->explists[fs->a->nexplists-1];
  luaM_growvector(fs->H, l->types, l->ntypes, l->sizetypes, TypeInfo, MAX_INT,
                  "");
  return &l->types[l->ntypes++];
}


/*
** push a TypeInfo element to the top ExpListEntry's TypeInfo stack using the
** type information from the expression V
*/
static void pushexptyping (FuncState *fs, expdesc *v) {
  TypeInfo *t = pushexplisttypeinfo(fs);
  t->type = exptype(v);
  t->proto = expproto(v);
  t->is_static = exptype(v) != LUA_TNONE;
}


/*
** push a TypeInfo element to the top ExpListEntry's TypeInfo stack using the
** type information from TYPE
*/
static void pushpaddedtyping (FuncState *fs, int type) {
  TypeInfo *t = pushexplisttypeinfo(fs);
  lua_assert(type != LUA_TSTRUCT);
  t->type = type;
  t->proto = NULL;
  t->is_static = (type != LUA_TNONE);
}


/*
** pop a TypeInfo element from the top ExpListEntry's TypeInfo stack and apply
** the type information to the expression V
*/
static void popexptyping (FuncState *fs, expdesc *v) {
  ExpListEntry *l = gettopexplistentry(fs);
  TypeInfo *t;
  lua_assert(l->ntypes > 0);
  t = &l->types[--l->ntypes];
  exptype(v) = t->type;
  expproto(v) = t->proto;
}


/*
** remove extra TypeInfo entries from the top expression list
*/
static void popnlisttyping (FuncState *fs, int n) {
  ExpListEntry *l = gettopexplistentry(fs);
  lua_assert(l->ntypes >= n);
  l->ntypes -= n;
}


#if HKSC_STRUCTURE_EXTENSION_ON
/*
** push a new type constraint to the top ExpListEntry
*/
static void pushtypeconstraint (FuncState *fs, ExpListEntry *l, TypeInfo *t) {
  luaM_growvector(fs->H, l->constraints, l->nconstraints, l->sizeconstraints,
                  TypeInfo, MAX_INT, "");
  l->constraints[l->nconstraints++] = *t;
}

/*
** return the top TypeInfo element in the type constraints for an ExpListEntry
*/
static TypeInfo *poptypeconstraint (ExpListEntry *l) {
  lua_assert(l->hasconstraints);
  lua_assert(l->nconstraints > 0);
  return &l->constraints[--l->nconstraints];
}


/*
** pop a type constraint from the stack and apply it to an expression
*/
static void applytypeconstraint (FuncState *fs, expdesc *e) {
  ExpListEntry *l = gettopexplistentry(fs);
  lua_assert(l->nconstraints >= 0);
  /* Havok Script has a bug here where it crashes if there are extra values on
     the right hand side of a typed local definition, such as the following:
        local i:ifunction = function() end, "", ""
     The type-constraint handler just checks that `hasconstraints' is set,
     without checking if there are any type constraint entries left in the
     array, which there won't be after storing the closure and popping the
     type constraint for the local variable i; the now empty stack is still
     accessed to handle the 2 empty strings because `hasconstraints' remains
     set, but the buffer pointer is NULL, and a segmentation fault occurs */
  if (l->hasconstraints && l->nconstraints != 0)
    luaK_applytypecontraint(fs, poptypeconstraint(l), e);
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


static int adjust_assign (LexState *ls, int nvars, int nexps, expdesc *e) {
  FuncState *fs = ls->fs;
  int extra = nvars - nexps;
  if (hasmultret(e->k)) {
#if HKSC_STRUCTURE_EXTENSION_ON
    ExpListEntry *l = gettopexplistentry(fs);
    int checktypereg;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    extra++;  /* includes call itself */
    if (extra < 0) extra = 0;
#if HKSC_STRUCTURE_EXTENSION_ON
    /* vararg expressions will go into the next register, whereas function
       calls have already reserved a slot for the result */
    checktypereg = fs->freereg - (e->k != VVARARG);
    if (l->hasconstraints)
      /* vararg or a function call cannot be statically type-checked */
      luaK_checktype(fs, poptypeconstraint(l), checktypereg);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    luaK_setreturns(fs, e, extra);  /* last exp. provides the difference */
    if (extra > 1) {
#if HKSC_STRUCTURE_EXTENSION_ON
      if (l->hasconstraints) {
        int i;
        for (i = 0; i < extra-1; i++)
          luaK_checktype(fs, poptypeconstraint(l), ++checktypereg);
      }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
      luaK_reserveregs(fs, extra-1);
    }
  }
  else {
    if (e->k != VVOID) {
      luaK_exp2nextreg(fs, e);  /* close last expression */
#if HKSC_STRUCTURE_EXTENSION_ON
      applytypeconstraint(fs, e);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    }
    if (extra > 0) {
      int reg = fs->freereg;
      luaK_reserveregs(fs, extra);
      luaK_nil(fs, reg, extra);
      return 1;
    }
  }
  return 0;
}


static void enterlevel (LexState *ls) {
  if (++ls->H->nCcalls > LUAI_MAXCCALLS)
	luaX_lexerror(ls, "chunk has too many syntax levels", 0);
}


#define leavelevel(ls)	((ls)->H->nCcalls--)


static void enterblock (FuncState *fs, BlockCnt *bl, lu_byte isbreakable) {
  bl->breaklist = NO_JUMP;
  bl->isbreakable = isbreakable;
  bl->nactvar = fs->nactvar;
  bl->upval = 0;
  bl->previous = fs->bl;
  fs->bl = bl;
  /* I commented out the assertion below because there is a case where it can
     not be true, namely, when a list for-loop has more than 3 expressions in
     its list. This is still a valid assertion and it is a bug in Lua 5.1. The
     bug was fixed in Lua 5.3.4, and in the official documentation, the bug is
     said to have existed since Lua 5.2. This bug exists in Havok Script as
     well. */
  /*lua_assert(fs->freereg == fs->nactvar);*/
}


static void leaveblock (FuncState *fs) {
  BlockCnt *bl = fs->bl;
  fs->bl = bl->previous;
  removevars(fs->ls, bl->nactvar);
  if (bl->upval)
    luaK_codeABC(fs, OP_CLOSE, bl->nactvar, 0, 0);
  /* a block either controls scope or breaks (never both) */
  lua_assert(!bl->isbreakable || !bl->upval);
  lua_assert(bl->nactvar == fs->nactvar);
  fs->freereg = fs->nactvar;  /* free registers */
  luaK_patchtohere(fs, bl->breaklist);
}


static void pushclosure (LexState *ls, FuncState *func, expdesc *v) {
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  int oldsize = f->sizep;
  int i;
  luaM_growvector(ls->H, f->p, fs->np, f->sizep, Proto *,
                  MAXARG_Bx, "constant table overflow");
  while (oldsize < f->sizep) f->p[oldsize++] = NULL;
  f->p[fs->np++] = func->f;
  init_typed_exp(v, VRELOCABLE, luaK_codeABx(fs, OP_CLOSURE, 0, fs->np-1),
                 LUA_TIFUNCTION);
  for (i=0; i<func->f->nups; i++) {
    int a = (func->upvalues[i].k == VLOCAL) ? 1 : 2;
    luaK_codeABx(fs, OP_DATA, a, func->upvalues[i].info);
  }
}

/* name-part types */
#define NAMEPART_NONE  (-1)
#define NAMEPART_NAME  0 /* regular variable name */
#define NAMEPART_FIELD 1 /* field name */
#define NAMEPART_SELF  2 /* self field name */

typedef struct NamePart {
  TString *name;
  int type;
} NamePart;

struct FunctionNameStack {
  struct NamePart *names;
  int used;
  int alloc;
};

#define MAX_FUNCNAME 512

/* add a name to the current list of name parts */
static void addnamepart (LexState *ls, TString *name, int type) {
  NamePart *namepart;
  struct FunctionNameStack *stk = ls->funcnamestack;
  lua_assert(stk != NULL);
  lua_assert(type != NAMEPART_NONE);
  if (type == NAMEPART_NAME) /* regular names can only be first in the list */
    lua_assert(stk->used == 0);
  else
    lua_assert(stk->used > 0);
  lua_assert(stk->used <= stk->alloc);
  if (stk->used >= (MAX_FUNCNAME/2))
    return; /* it won't be used anyway */
  if (stk->used == stk->alloc)
    luaM_growvector(ls->H, stk->names, stk->used, stk->alloc, NamePart,
                    MAX_INT, "too many parts to function name");
  namepart = &stk->names[stk->used++];
  namepart->name = name;
  namepart->type = type;
}


/* free the entire name part stack */
static void freefuncnamestack (LexState *ls) {
  struct FunctionNameStack *stk = ls->funcnamestack;
  lua_assert(stk != NULL);
  if (stk->names != NULL)
    luaM_freearray(ls->H, stk->names, stk->alloc, NamePart);
  stk->names = NULL;
  stk->alloc = stk->used = 0;
}


/*
** build a function name from the current name part stack
** This function in Havok Script has 2 bugs. One of them is fatal, a buffer
** overrun vulnerability, and cannot be preserved. The other is a benign one
** which causes too-long function names to contain an embedded null terminator
*/
static TString *buildfuncname (LexState *ls) {
  char buff[MAX_FUNCNAME];
  int i;
  size_t len = 0;
  size_t namelen;
  struct FunctionNameStack *stk = ls->funcnamestack;
  lua_assert(stk != NULL);
  for (i = 0; i < stk->used; i++) {
    size_t l;
    TString *name = stk->names[i].name;
    int type = stk->names[i].type;
    lua_assert(name != NULL);
    if (type == NAMEPART_FIELD)
      buff[len++] = '.';
    else if (type == NAMEPART_SELF)
      buff[len++] = ':';
    l = name->tsv.len;
    if (l > MAX_FUNCNAME - len)
      l = MAX_FUNCNAME - len;
    memcpy(buff+len,getstr(name),l);
    len+=l;
    /* I added this to prevent buffer overrun. This check is not done in
       Havok Script. */
    if (len >= MAX_FUNCNAME)
      break;
  }
  if (len >= MAX_FUNCNAME)
    namelen = MAX_FUNCNAME - 1;
  else
    namelen = len;
  buff[namelen] = '\0';
#ifndef HKSC_PRESERVE_HAVOKSCRIPT_BUGS
  len = namelen;
#else /* HKSC_PRESERVE_HAVOKSCRIPT_BUGS */
  /* instead of setting len to namelen, which guarantees that the null byte
     will not be included in the generated string, len is allowed to be exactly
     MAX_FUNCNAME to match the bug in Havok Script where the null byte gets
     embedded in the function name if it is as long as or longer than the
     maximum allowed length */
  if (len > MAX_FUNCNAME)
    len = MAX_FUNCNAME;
#endif /* HKSC_PRESERVE_HAVOKSCRIPT_BUGS */
  stk->used = 0; /* discharge name parts */
  if (len != 0)
    return luaS_newlstr(ls->H, buff, len);
  else
    return NULL;
}


static TypeAnalyzer *newtypeanalyzer(hksc_State *H) {
  TypeAnalyzer *a = luaM_new(H, TypeAnalyzer);
  luaC_link(H, obj2gco(a), LUA_TTYPEANALYZER);
  a->locvarstyping = NULL;
  a->sizelocvarstyping = 0;
  a->lhstyping = NULL;
  a->sizelhstyping = 0;
  a->explists = NULL;
  a->sizeexplists = 0;
  a->nexplists = 0;
  return a;
}


static void freetypeanalyzerdata(hksc_State *H, TypeAnalyzer *a) {
  int i;
  luaM_freearray(H, a->locvarstyping, a->sizelocvarstyping, TypeInfo);
  a->locvarstyping = NULL; a->sizelocvarstyping = 0;
  luaM_freearray(H, a->lhstyping, a->sizelhstyping, TypeInfo);
  a->lhstyping = NULL; a->sizelhstyping = 0;
  for (i = 0; i < a->nexplists; i++) {
    ExpListEntry *l = &a->explists[i];
    luaM_freearray(H, l->types, l->sizetypes, TypeInfo);
    if (l->hasconstraints)
      luaM_freearray(H, l->constraints, l->sizeconstraints, TypeInfo);
  }
  luaM_freearray(H, a->explists, a->sizeexplists, ExpListEntry);
  a->explists = NULL; a->nexplists = a->sizeexplists = 0;
}

/*
** called by garbage collector
*/
void luaY_freetypeanalyzer(hksc_State *H, TypeAnalyzer *a) {
  freetypeanalyzerdata(H, a);
  luaM_free(H, a);
}


static void open_func (LexState *ls, FuncState *fs) {
  TString *name;
  hksc_State *H = ls->H;
  Proto *f = luaF_newproto(H);
  fs->f = f;
  if (ls->fs == NULL) /* main chunk */
    name = luaS_newliteral(ls->H, MAINCHUNKNAME);
  else
    name = buildfuncname(ls);
  fs->a = newtypeanalyzer(H);
  fs->prev = ls->fs;  /* linked list of funcstates */
  fs->ls = ls;
  fs->H = H;
  ls->fs = fs;
  fs->pc = 0;
  fs->lasttarget = -1;
  fs->jpc = NO_JUMP;
  fs->freereg = 0;
  fs->nk = 0;
  fs->np = 0;
  fs->nlocvars = 0;
  fs->nlocalslhs = 0;
  fs->nactvar = 0;
  fs->bl = NULL;
  f->source = ls->source;
  f->name = name;
  f->maxstacksize = 2;  /* registers 0/1 are always valid */
  fs->h = luaH_new(H, 0, 0);
}


static void close_func (LexState *ls) {
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  removevars(ls, 0);
  luaK_ret(fs, 0, 0);  /* final return */
  luaM_reallocvector(ls->H, f->code, f->sizecode, fs->pc, Instruction);
  f->sizecode = fs->pc;
  luaM_reallocvector(ls->H, f->lineinfo, f->sizelineinfo, fs->pc, int);
  f->sizelineinfo = fs->pc;
  luaM_reallocvector(ls->H, f->k, f->sizek, fs->nk, TValue);
  f->sizek = fs->nk;
  luaM_reallocvector(ls->H, f->p, f->sizep, fs->np, Proto *);
  f->sizep = fs->np;
  luaM_reallocvector(ls->H, f->locvars, f->sizelocvars, fs->nlocvars, LocVar);
  f->sizelocvars = fs->nlocvars;
  luaM_reallocvector(ls->H, f->upvalues, f->sizeupvalues, f->nups, TString *);
  f->sizeupvalues = f->nups;
#ifdef LUA_CODT6
  luaF_generatehash(ls->H,f);
#endif /* LUA_CODT6 */
  lua_assert(luaG_checkcode(f));
  lua_assert(fs->bl == NULL);
  ls->fs = fs->prev;
  killtemp(obj2gco(fs->h)); /* make table collectable */
  /* free the type analyzer arrays and mark it for collection */
  freetypeanalyzerdata(ls->H, fs->a);
  killtemp(obj2gco(fs->a));
  /* last token read was anchored in defunct function; must reanchor it */
  if (fs) anchor_token(ls);
}


#if HKSC_GETGLOBAL_MEMOIZATION
static void check_memo_testing_mode (LexState *ls) {
  if (Settings(ls->H).skip_memo)
    luaK_ret(ls->fs, 0, 0);
}
#else /* !HKSC_GETGLOBAL_MEMOIZATION */
#define check_memo_testing_mode(ls) ((void)0)
#endif /* HKSC_GETGLOBAL_MEMOIZATION */


static void parser_inner_func (hksc_State *H, void *ud) {
  struct LexState *ls = cast(void **, ud)[0];
  struct FuncState *fs = cast(void **, ud)[1];
  open_func(ls, fs);
  fs->f->is_vararg = VARARG_ISVARARG;  /* main func. is always vararg */
  luaX_readfirsttoken(ls);  /* read first token */
  switch (ls->t.token) {
    case TK_BOM_UTF8:
      ls->textmode = UTF8;
      luaX_next(ls);
      break;
    case TK_BOM_INVALID:
    case TK_BOM_UTF16LE:
    case TK_BOM_UTF16BE:
    case TK_BOM_UTF32LE:
    case TK_BOM_UTF32BE:
      luaX_inputerror(ls, "Invalid or unsupported file encoding. Only ASCII "
                      "and UTF-8 are supported");
      break;
  }
  check_memo_testing_mode(ls);
  chunk(ls);
  check(ls, TK_EOS);
  close_func(ls);
  UNUSED(H);
}


Proto *luaY_parser (hksc_State *H, ZIO *z, Mbuffer *buff, const char *name) {
  void *data [2];
  int status;
  struct LexState lexstate;
  struct FuncState funcstate;
  struct FunctionNameStack funcnamestack;
  data[0] = &lexstate, data[1] = &funcstate;
  lexstate.buff = buff;
  lexstate.funcnamestack = &funcnamestack;
  funcnamestack.names = NULL;
  funcnamestack.used = funcnamestack.alloc = 0;
  luaX_setinput(H, &lexstate, z, luaS_new(H, name));
  status = luaD_pcall(H, parser_inner_func, data);
  freefuncnamestack(&lexstate);
  if (status)
    luaD_throw(H, status); /* return the error to the outer pcall */
  luaK_optimize_function(H, funcstate.f);
  lua_assert(funcstate.prev == NULL);
  lua_assert(funcstate.f->nups == 0);
  lua_assert(lexstate.fs == NULL);
  return funcstate.f;
}



/*============================================================*/
/* GRAMMAR RULES */
/*============================================================*/


static void field (LexState *ls, expdesc *v, int type) {
  /* field -> ['.' | ':'] NAME */
  FuncState *fs = ls->fs;
  TString *name;
  expdesc key;
  luaK_exp2anyreg(fs, v);
  luaX_next(ls);  /* skip the dot or colon */
  name = str_checkname(ls);
  codestring(ls, &key, name);
  if (type != NAMEPART_NONE) {
    lua_assert(type == NAMEPART_FIELD || type == NAMEPART_SELF);
    addnamepart(ls, name, type); /* add the name part to the chain */
  }
  luaK_indexed(fs, v, &key);
}


static void yindex (LexState *ls, expdesc *v) {
  /* index -> '[' expr ']' */
  luaX_next(ls);  /* skip the '[' */
  expr(ls, v);
  luaK_exp2val(ls->fs, v);
  checknext(ls, ']');
}


/*
** {======================================================================
** Rules for Constructors
** =======================================================================
*/


struct ConsControl {
  expdesc v;  /* last list item read */
  expdesc *t;  /* table descriptor */
  int nh;  /* total number of `record' elements */
  int na;  /* total number of array elements */
  int tostore;  /* number of array elements pending to be stored */
  struct StructProto *proto;  /* prototype of struct instance being created */
};


#if HKSC_STRUCTURE_EXTENSION_ON

static void invalid_slot (LexState *ls, StructProto *proto, TString *name) {
  const char *msg = luaO_pushfstring(ls->H, "Attempt to assign slot '%s' on "
            "instances of " LUA_QS_U " is not allowed.", getstr(name),
            getstr(proto->name));
  luaX_semerror(ls, msg);
}


/*
** describes a slot intiailization in a struct constructor
*/
struct slotfield_s {
  StructSlot *slot;
  TypeInfo t;
};


static int checkslotfield (LexState *ls, StructProto *proto, expdesc *key,
                           expdesc *val, struct slotfield_s *s) {
  switch (key->k) {
    case VNIL:
      luaX_semerror(ls, "Table index is nil.");
      break;
    case VTRUE:
    case VFALSE:
    case VKNUM:
      if (proto->hasproxy == 0)
        type_error(ls, "Attempt to assign to number indexes on instances of "
                   "structure '%s' is not allowed.", proto->name);
      return ASSIGN_SLOT_PROXYTABLE;
    case VK:
      if (ttisstring(&ls->fs->f->k[key->u.s.info])) {
        TString *slotname = rawtsvalue(&ls->fs->f->k[key->u.s.info]);
        StructSlot *slot = luaR_findslot(proto, slotname);
        if (slot != NULL) {
          luaK_getslottypeinfo(ls->fs, slot, &s->t);
          s->slot = slot;
          return luaK_checkslotassignment(ls->fs, slotname, val, &s->t);
        }
        if (proto->hasproxy == 0)
          invalid_slot(ls, proto, slotname);
        return ASSIGN_SLOT_PROXYTABLE;
      }
      break;
    case VRELOCABLE:
    case VNONRELOC:
      if (exptype(key) == LUA_TNONE || exptype(key) == LUA_TSTRING)
        return ASSIGN_SLOT_GENERIC;
      break;
    default:
      return ASSIGN_SLOT_GENERIC;
  }
  if (proto->hasproxy == 0)
    type_error(ls, "Attempt to assign to non-string indexes on instances "
                   "of structure '%s' is not allowed.", proto->name);
  return ASSIGN_SLOT_PROXYTABLE;
}

static void checkslotname (LexState *ls, StructProto *proto, TString *name) {
  if (Settings(ls->H).emit_struct == 0)
    return;
  if (proto && luaR_findslot(proto, name) == NULL && proto->hasproxy == 0)
    invalid_slot(ls, proto, name);
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


static void recfield (LexState *ls, struct ConsControl *cc) {
  /* recfield -> (NAME | `['exp1`]') = exp1 */
  FuncState *fs = ls->fs;
  int reg = ls->fs->freereg;
  expdesc key, val;
  int rkkey, rkval;
  if (ls->t.token == TK_NAME) {
    luaY_checklimit(fs, cc->nh, MAX_INT, "items in a constructor");
#if HKSC_STRUCTURE_EXTENSION_ON
    checkslotname(ls, cc->proto, ls->t.seminfo.ts);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    checkname(ls, &key);
  }
  else  /* ls->t.token == '[' */
    yindex(ls, &key);
  cc->nh++;
  checknext(ls, '=');
  rkkey = luaK_exp2RK(fs, &key);
  expr(ls, &val);
  rkval = luaK_exp2RK(fs, &val);
#if HKSC_STRUCTURE_EXTENSION_ON
  if (Settings(ls->H).emit_struct && cc->proto != NULL) {
    struct slotfield_s s = {NULL, typeinfoinit(LUA_TNONE, NULL, 1)};
    int res = checkslotfield(ls, cc->proto, &key, &val, &s);
    /* only allocate hashtable space for proxytable fields */
    if (res != ASSIGN_SLOT_PROXYTABLE) cc->nh--;
    luaK_setslot(fs, s.slot, &s.t, cc->t->u.s.info, rkkey, rkval, res);
  }
  else
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  luaK_codeABC(fs, OP_SETTABLE, cc->t->u.s.info, rkkey, rkval);
  fs->freereg = reg;  /* free registers */
}


static void closelistfield (FuncState *fs, struct ConsControl *cc) {
  if (cc->v.k == VVOID) return;  /* there is no list item */
  luaK_exp2nextreg(fs, &cc->v);
  cc->v.k = VVOID;
  if (cc->tostore == LFIELDS_PER_FLUSH) {
    luaK_setlist(fs, cc->t->u.s.info, cc->na, cc->tostore);  /* flush */
    cc->tostore = 0;  /* no more items pending */
  }
}


static void lastlistfield (FuncState *fs, struct ConsControl *cc) {
  if (cc->tostore == 0) return;
  if (hasmultret(cc->v.k)) {
    luaK_setmultret(fs, &cc->v);
    luaK_setlist(fs, cc->t->u.s.info, cc->na, LUA_MULTRET);
    cc->na--;  /* do not count last expression (unknown number of elements) */
  }
  else {
    if (cc->v.k != VVOID)
      luaK_exp2nextreg(fs, &cc->v);
    luaK_setlist(fs, cc->t->u.s.info, cc->na, cc->tostore);
  }
}


static void listfield (LexState *ls, struct ConsControl *cc) {
#if HKSC_STRUCTURE_EXTENSION_ON
  if (cc->proto && cc->proto->hasproxy == 0 && Settings(ls->H).emit_struct) {
    type_error(ls, "Attempt to assign to number indexes on instances of "
               "structure '%s' is not allowed.", cc->proto->name);
  }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  expr(ls, &cc->v);
  luaY_checklimit(ls->fs, cc->na, MAX_INT, "items in a constructor");
  cc->na++;
  cc->tostore++;
}


static int codeconstructor (LexState *ls, expdesc *t) {
  FuncState *fs = ls->fs;
  int pc;
#if HKSC_STRUCTURE_EXTENSION_ON
  if (Settings(ls->H).emit_struct == 0 || ls->cons_proto == NULL)
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  {
    pc = luaK_codeABC(fs, OP_NEWTABLE, 0, 0, 0);
    init_typed_exp(t, VRELOCABLE, pc, LUA_TTABLE);
  }
#if HKSC_STRUCTURE_EXTENSION_ON
  else {  /* constructing an struct instance */
    StructProto *p = ls->cons_proto;
    pc = luaK_codeABC(fs, OP_NEWSTRUCT, 0, 0, 0);
    luaK_codeABx(fs, OP_DATA, 0, p->id);
    init_typed_exp(t, VRELOCABLE, pc, LUA_TSTRUCT);
    expproto(t) = p;
    ls->cons_proto = NULL;
  }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  return pc;
}


#ifdef HKSC_TABLESIZE_EXTENSION
static void numconstexpr (LexState *ls, expdesc *v);

static void constructorsizes (LexState *ls, struct ConsControl *cc) {
  if (testnext(ls, ':')) {
    int na = cc->na, nh = cc->nh;
    expdesc e;
    if (testnext(ls, ','))
      goto gethashsize;
    numconstexpr(ls, &e);
    na = cast_int(e.u.nval);
    if (testnext(ls, ',')) {
      gethashsize: numconstexpr(ls, &e);
      nh = cast_int(e.u.nval);
    }
#if HKSC_STRUCTURE_EXTENSION_ON
    if (Settings(ls->H).emit_struct && cc->proto && !cc->proto->hasproxy)
      return;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    if (na > cc->na)
      cc->na = na;
    if (nh > cc->nh)
      cc->nh = nh;
  }
}
#endif /* HKSC_TABLESIZE_EXTENSION */


static void constructor (LexState *ls, expdesc *t) {
  /* constructor -> ?? */
  FuncState *fs = ls->fs;
  int line = ls->linenumber;
  int pc = codeconstructor(ls, t);
  /*int pc = luaK_codeABC(fs, OP_NEWTABLE, 0, 0, 0);*/
  struct ConsControl cc;
  cc.na = cc.nh = cc.tostore = 0;
  cc.t = t;
  cc.proto = expproto(t);
  init_exp(&cc.v, VVOID, 0);  /* no value (yet) */
  luaK_exp2nextreg(ls->fs, t);  /* fix it at stack top (for gc) */
  checknext(ls, '{');
  do {
    lua_assert(cc.v.k == VVOID || cc.tostore > 0);
    if (ls->t.token == '}') break;
    closelistfield(fs, &cc);
    switch(ls->t.token) {
      case TK_NAME: {  /* may be listfields or recfields */
        luaX_lookahead(ls);
        if (ls->lookahead.token != '=')  /* expression? */
          listfield(ls, &cc);
        else
          recfield(ls, &cc);
        break;
      }
      case '[': {  /* constructor_item -> recfield */
        recfield(ls, &cc);
        break;
      }
      default: {  /* constructor_part -> listfield */
        listfield(ls, &cc);
        break;
      }
    }
  } while (testnext(ls, ',') || testnext(ls, ';'));
  check_match(ls, '}', '{', line);
  lastlistfield(fs, &cc);
#ifdef HKSC_TABLESIZE_EXTENSION
  constructorsizes(ls, &cc);
#endif /* HKSC_TABLESIZE_EXTENSION */
  SETARG_B(fs->f->code[pc], luaO_int2fb(cc.na)); /* set initial array size */
  SETARG_C(fs->f->code[pc], luaO_int2fb(cc.nh));  /* set initial table size */
}


#if HKSC_STRUCTURE_EXTENSION_ON
static void makeconstructor (LexState *ls, expdesc *t) {
  TString *name;
  luaX_next(ls);  /* skip HMAKE */
  name = str_checkname(ls);
  if (Settings(ls->H).emit_struct) {
    StructProto *p = luaR_getstructbyname(ls->H, name);
    if (p == NULL)
      type_error(ls, "Attempt to reference an undefined structure '%s'.",name);
    ls->cons_proto = p;
  }
  constructor(ls, t);
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


/* }====================================================================== */



static void parlist (LexState *ls) {
  /* parlist -> [ param { `,' param } ] */
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  int nparams = 0;
  f->is_vararg = 0;
  if (ls->t.token != ')') {  /* is `parlist' not empty? */
    do {
      switch (ls->t.token) {
        case TK_NAME: {  /* param -> NAME */
          luaX_lookahead(ls);
          if (ls->lookahead.token == ':') {
#if HKSC_STRUCTURE_EXTENSION_ON
            TString *varname = str_checkname(ls);
            luaX_next(ls);  /* skip `:' */
            typedlocvar(ls, varname, str_checkname(ls), nparams);
#else /* !HKSC_STRUCTURE_EXTENSION_ON */
            error_typedparam(ls);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
          }
          else {
            new_localvar(ls, str_checkname(ls), nparams);
          }
          nparams++;
          break;
        }
        case TK_DOTS: {  /* param -> `...' */
          luaX_next(ls);
#if defined(LUA_COMPAT_VARARG)
          /* use `arg' as default name */
          new_localvarliteral(ls, "arg", nparams++);
          f->is_vararg = VARARG_HASARG | VARARG_NEEDSARG;
#endif
          f->is_vararg |= VARARG_ISVARARG;
          break;
        }
        default: luaX_syntaxerror(ls, "<name> or " LUA_QL("...") " expected");
      }
    } while (!f->is_vararg && testnext(ls, ','));
  }
  adjustlocalvars(ls, nparams);
  f->numparams = cast_byte(fs->nactvar - (f->is_vararg & VARARG_HASARG));
  luaK_reserveregs(fs, fs->nactvar);  /* reserve register for parameters */
}


#if HKSC_STRUCTURE_EXTENSION_ON
static void parchecktype (LexState *ls) {
  FuncState *fs = ls->fs;
  int i;
  for (i = 0; i < cast_int(fs->nactvar); i++)
    luaK_checktype(fs, &fs->a->locvarstyping[i], i);
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


static void body (LexState *ls, expdesc *e, int needself, int line) {
  /* body ->  `(' parlist `)' chunk END */
  FuncState new_fs;
  open_func(ls, &new_fs);
  new_fs.f->linedefined = line;
  checknext(ls, '(');
  if (needself) {
    new_localvarliteral(ls, "self", 0);
    adjustlocalvars(ls, 1);
  }
  parlist(ls);
  checknext(ls, ')');
#if HKSC_STRUCTURE_EXTENSION_ON
  parchecktype(ls);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  check_memo_testing_mode(ls);
  chunk(ls);
  new_fs.f->lastlinedefined = ls->linenumber;
  check_match(ls, TK_END, TK_FUNCTION, line);
  close_func(ls);
  pushclosure(ls, &new_fs, e);
}


static int explist1 (LexState *ls, expdesc *v) {
  /* explist1 -> expr { `,' expr } */
  int n = 1;  /* at least one expression */
  expr(ls, v);
  while (testnext(ls, ',')) {
    luaK_exp2nextreg(ls->fs, v);
#if HKSC_STRUCTURE_EXTENSION_ON
    applytypeconstraint(ls->fs, v);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    pushexptyping(ls->fs, v);
    expr(ls, v);
    n++;
  }
  return n;
}


static int explist1_notyping (LexState *ls, expdesc *v) {
  int n;
  pushexplistentry(ls->fs);
  n = explist1(ls, v);
  popexplistentry(ls->fs);
  return n;
}


static void funcargs (LexState *ls, expdesc *f) {
  OpCode o; /* opcode to use in the call instruction */
  FuncState *fs = ls->fs;
  expdesc args;
  int base, nparams;
  int line = ls->linenumber;
  switch (ls->t.token) {
    case '(': {  /* funcargs -> `(' [ explist1 ] `)' */
      if (line != ls->lastline)
        luaX_syntaxerror(ls,
                         "ambiguous syntax (function call x new statement)");
      luaX_next(ls);
      if (ls->t.token == ')')  /* arg list is empty? */
        args.k = VVOID;
      else {
        explist1_notyping(ls, &args);
        luaK_setmultret(fs, &args);
      }
      check_match(ls, ')', '(', line);
      break;
    }
    case '{': {  /* funcargs -> constructor */
      constructor(ls, &args);
      break;
    }
    case TK_STRING: {  /* funcargs -> STRING */
      codestring(ls, &args, ls->t.seminfo.ts);
      luaX_next(ls);  /* must use `seminfo' before `next' */
      break;
    }
    default: {
      luaX_syntaxerror(ls, "function arguments expected");
      return;
    }
  }
  lua_assert(f->k == VNONRELOC);
  base = f->u.s.info;  /* base register for call */
  if (hasmultret(args.k))
    nparams = LUA_MULTRET;  /* open call */
  else {
    if (args.k != VVOID)
      luaK_exp2nextreg(fs, &args);  /* close last argument */
    nparams = fs->freereg - (base+1);
  }
  o = (f->inferred_type == LUA_TIFUNCTION) ? OP_CALL_I :
      (f->inferred_type == LUA_TCFUNCTION) ? OP_CALL_C : OP_CALL;
  init_exp(f, VCALL, luaK_codeABC(fs, o, base, nparams+1, 2));
  luaK_fixline(fs, line);
  fs->freereg = base+1;  /* call remove function and arguments and leaves
                            (unless changed) one result */
}




/*
** {======================================================================
** Expression parsing
** =======================================================================
*/


static void prefixexp (LexState *ls, expdesc *v) {
  /* prefixexp -> NAME | '(' expr ')' */
  switch (ls->t.token) {
    case '(': {
      int line = ls->linenumber;
      luaX_next(ls);
      expr(ls, v);
      check_match(ls, ')', '(', line);
      luaK_dischargevars(ls->fs, v);
      return;
    }
    case TK_NAME: {
      singlevar(ls, v);
      return;
    }
    default: {
      luaX_syntaxerror(ls, "unexpected symbol");
      return;
    }
  }
}


static void primaryexp (LexState *ls, expdesc *v) {
  /* primaryexp ->
        prefixexp { `.' NAME | `[' exp `]' | `:' NAME funcargs | funcargs } */
  FuncState *fs = ls->fs;
  prefixexp(ls, v);
  for (;;) {
    switch (ls->t.token) {
      case '.': {  /* field */
        field(ls, v, NAMEPART_NONE);
        break;
      }
      case '[': {  /* `[' exp1 `]' */
        expdesc key;
        luaK_exp2anyreg(fs, v);
        yindex(ls, &key);
        luaK_indexed(fs, v, &key);
        break;
      }
      case ':': {  /* `:' NAME funcargs */
        expdesc key;
        luaX_next(ls);
        checkname(ls, &key);
        luaK_self(fs, v, &key);
        funcargs(ls, v);
        break;
      }
      case '(': case TK_STRING: case '{': {  /* funcargs */
        luaK_exp2nextreg(fs, v);
        funcargs(ls, v);
        break;
      }
      default: return;
    }
  }
}


static void simpleexp (LexState *ls, expdesc *v) {
  /* simpleexp -> NUMBER | STRING | NIL | true | false | ... |
                  HMAKE NAME constructor | constructor | FUNCTION body |
                  primaryexp */
  switch (ls->t.token) {
    case TK_NUMBER: {
      init_typed_exp(v, VKNUM, 0, LUA_TNUMBER);
      v->u.nval = ls->t.seminfo.r;
      break;
    }
    case TK_LITERALLUD:
    case TK_LITERALUI64: {
      codeliteral(ls, v, ls->t.seminfo.l, ls->t.token);
      break;
    }
    case TK_STRING: {
      codestring(ls, v, ls->t.seminfo.ts);
      break;
    }
    case TK_NIL: {
      init_typed_exp(v, VNIL, 0, LUA_TNIL);
      break;
    }
    case TK_TRUE: {
      init_typed_exp(v, VTRUE, 0, LUA_TBOOLEAN);
      break;
    }
    case TK_FALSE: {
      init_typed_exp(v, VFALSE, 0, LUA_TBOOLEAN);
      break;
    }
    case TK_DOTS: {  /* vararg */
      FuncState *fs = ls->fs;
      check_condition(ls, fs->f->is_vararg,
                    "cannot use " LUA_QL("...") " outside a vararg function");
      fs->f->is_vararg &= ~VARARG_NEEDSARG;  /* don't need 'arg' */
      init_exp(v, VVARARG, luaK_codeABC(fs, OP_VARARG, 0, 1, 0));
      break;
    }
#if HKSC_STRUCTURE_EXTENSION_ON
    case TK_MAKE: {
      makeconstructor(ls, v);
      return;
    }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    case '{': {  /* constructor */
      constructor(ls, v);
      return;
    }
    case TK_FUNCTION: {
      luaX_next(ls);
      body(ls, v, 0, ls->linenumber);
      return;
    }
    default: {
      primaryexp(ls, v);
      return;
    }
  }
  luaX_next(ls);
}


static UnOpr getunopr (int op) {
  switch (op) {
    case TK_NOT: return OPR_NOT;
    case '-': return OPR_MINUS;
    case '#': return OPR_LEN;
    default: return OPR_NOUNOPR;
  }
}


static BinOpr getbinopr (int op) {
  switch (op) {
    case '+': return OPR_ADD;
    case '-': return OPR_SUB;
    case '*': return OPR_MUL;
    case '/': return OPR_DIV;
    case '%': return OPR_MOD;
    case '^': return OPR_POW;
    case TK_CONCAT: return OPR_CONCAT;
#ifdef LUA_CODT7 /* T7 extensions */
    case TK_LEFT_SHIFT: return OPR_LEFT_SHIFT;
    case TK_RIGHT_SHIFT: return OPR_RIGHT_SHIFT;
    case '&': return OPR_BIT_AND;
    case '|': return OPR_BIT_OR;
#endif /* LUA_CODT7 */
    case TK_NE: return OPR_NE;
    case TK_EQ: return OPR_EQ;
    case '<': return OPR_LT;
    case TK_LE: return OPR_LE;
    case '>': return OPR_GT;
    case TK_GE: return OPR_GE;
    case TK_AND: return OPR_AND;
    case TK_OR: return OPR_OR;
    default: return OPR_NOBINOPR;
  }
}

static const struct {
  lu_byte left;  /* left priority for each binary operator */
  lu_byte right; /* right priority */
} priority[] = {  /* ORDER OPR */
   {6, 6}, {6, 6}, {7, 7}, {7, 7}, {7, 7},  /* `+' `-' `/' `%' */
   {10, 9}, {5, 4},                 /* power and concat (right associative) */
#ifdef LUA_CODT7 /* T7 extensions */
   {5, 5}, {5, 5},                  /* shift left and shift right */
   {4, 4}, {4, 4},                  /* '&' and '|' */
#endif /* LUA_CODT7 */
   {3, 3}, {3, 3},                  /* equality and inequality */
   {3, 3}, {3, 3}, {3, 3}, {3, 3},  /* order */
   {2, 2}, {1, 1}                   /* logical (and/or) */
};

#define UNARY_PRIORITY	8  /* priority for unary operators */


/*
** subexpr -> (simpleexp | unop subexpr) { binop subexpr }
** where `binop' is any binary operator with a priority higher than `limit'
*/
static BinOpr subexpr (LexState *ls, expdesc *v, unsigned int limit) {
  BinOpr op;
  UnOpr uop;
  enterlevel(ls);
  uop = getunopr(ls->t.token);
  if (uop != OPR_NOUNOPR) {
    luaX_next(ls);
    subexpr(ls, v, UNARY_PRIORITY);
    luaK_prefix(ls->fs, uop, v);
  }
  else simpleexp(ls, v);
  /* expand while operators have priorities higher than `limit' */
  op = getbinopr(ls->t.token);
  while (op != OPR_NOBINOPR && priority[op].left > limit) {
    expdesc v2;
    BinOpr nextop;
    luaX_next(ls);
    luaK_infix(ls->fs, op, v);
    /* read sub-expression with higher priority */
    nextop = subexpr(ls, &v2, priority[op].right);
    luaK_posfix(ls->fs, op, v, &v2);
    op = nextop;
  }
  leavelevel(ls);
  return op;  /* return first untreated operator */
}


static void expr (LexState *ls, expdesc *v) {
  subexpr(ls, v, 0);
}


#ifdef HKSC_TABLESIZE_EXTENSION
static BinOpr numconstsubexpr (LexState *ls, expdesc *v, unsigned int limit) {
  BinOpr op;
  UnOpr uop;
  enterlevel(ls);
  uop = getunopr(ls->t.token);
  if (uop == OPR_MINUS) {
    luaX_next(ls);
    numconstexpr(ls, v);
  }
  else {
    if (uop != OPR_NOUNOPR)
      luaX_syntaxerror(ls,
                       "unexpected operator in constant numeric expression");
    check(ls, TK_NUMBER);
    simpleexp(ls, v);
  }
  lua_assert(v->k = VKNUM);
  /* expand while operators have priorities higher than `limit' */
  op = getbinopr(ls->t.token);
  while (op != OPR_NOBINOPR && priority[op].left > limit) {
    expdesc v2;
    BinOpr nextop;
    if (op == OPR_CONCAT || op >= OPR_NE)
      luaX_syntaxerror(ls,
                       "unexpected operator in constant numeric expression");
    luaX_next(ls);
    /* read sub-expression with higher priority */
    nextop = numconstsubexpr(ls, &v2, priority[op].right);
    luaK_posfix(ls->fs, op, v, &v2);
    op = nextop;
  }
  leavelevel(ls);
  return op;  /* return first untreated operator */
}

static void numconstexpr (LexState *ls, expdesc *v) {
  if (ls->t.token == '(') {
    int line = ls->linenumber;
    luaX_next(ls);
    numconstexpr(ls, v);
    check_match(ls, '(', ')', line);
    return;
  }
  numconstsubexpr(ls, v, 0);
}

#endif /* HKSC_TABLESIZE_EXTENSION */

/* }==================================================================== */



/*
** {======================================================================
** Rules for Statements
** =======================================================================
*/


static int block_follow (int token) {
  switch (token) {
    case TK_ELSE: case TK_ELSEIF: case TK_END:
    case TK_UNTIL: case TK_EOS:
      return 1;
    default: return 0;
  }
}


static void block (LexState *ls) {
  /* block -> chunk */
  FuncState *fs = ls->fs;
  BlockCnt bl;
  enterblock(fs, &bl, 0);
  chunk(ls);
  lua_assert(bl.breaklist == NO_JUMP);
  leaveblock(fs);
}


/*
** structure to chain all variables in the left-hand side of an
** assignment
*/
struct LHS_assign {
  struct LHS_assign *prev;
  expdesc v;  /* variable (global, local, upvalue, or indexed) */
};


/*
** check whether, in an assignment to a local variable, the local variable
** is needed in a previous assignment (to a table). If so, save original
** local value in a safe place and use this safe copy in the previous
** assignment.
*/
static void check_conflict (LexState *ls, struct LHS_assign *lh, expdesc *v) {
  FuncState *fs = ls->fs;
  int extra = fs->freereg;  /* eventual position to save local variable */
  int conflict = 0;
  for (; lh; lh = lh->prev) {
    if (lh->v.k == VINDEXED) {
      if (lh->v.u.s.info == v->u.s.info) {  /* conflict? */
        conflict = 1;
        lh->v.u.s.info = extra;  /* previous assignment will use safe copy */
      }
      if (lh->v.u.s.aux == v->u.s.info) {  /* conflict? */
        conflict = 1;
        lh->v.u.s.aux = extra;  /* previous assignment will use safe copy */
      }
    }
    else if (lh->v.k == VSLOT) {
      if (lh->v.u.s.info == v->u.s.info) {  /* conflict? */
        conflict = 1;
        lh->v.u.s.info = extra;  /* previous assignment will use safe copy */
      }
    }
  }
  if (conflict) {
    luaK_codeABC(fs, OP_MOVE, fs->freereg, v->u.s.info, 0);  /* make copy */
    luaK_reserveregs(fs, 1);
  }
}


#ifdef HKSC_MATCH_HAVOKSCRIPT_ERROR_MSG
# define NONVARIABLE_LHS_MESSAGE  \
  "non-variable on the right hand side of an assignment"
#else /* !HKSC_MATCH_HAVOKSCRIPT_ERROR_MSG */
# define NONVARIABLE_LHS_MESSAGE  \
  "non-variable on the left hand side of an assignment"
#endif /* HKSC_MATCH_HAVOKSCRIPT_ERROR_MSG */


static void assignment (LexState *ls, struct LHS_assign *lh, int nvars) {
  expdesc e;
  check_condition(ls, VLOCAL <= lh->v.k && lh->v.k <= VINDEXED,
                      "syntax error (" NONVARIABLE_LHS_MESSAGE ")");
  if (testnext(ls, ',')) {  /* assignment -> `,' primaryexp assignment */
    struct LHS_assign nv;
    nv.prev = lh;
    primaryexp(ls, &nv.v);
    if (nv.v.k == VLOCAL)
      check_conflict(ls, lh, &nv.v);
    luaY_checklimit(ls->fs, nvars, LUAI_MAXCCALLS - ls->H->nCcalls,
                    "variables in assignment");
    assignment(ls, &nv, nvars+1);
  }
  else {  /* assignment -> `=' explist1 */
    int nexps;
    checknext(ls, '=');
    pushexplistentry(ls->fs);
    nexps = explist1(ls, &e);
    if (nexps != nvars) {
      int filledwithnil = adjust_assign(ls, nvars, nexps, &e);
      pushexptyping(ls->fs, &e);
      if (nexps > nvars) {
        int extra = nexps - nvars;
        ls->fs->freereg -= extra;  /* remove extra values */
        popnlisttyping(ls->fs, extra);  /* remove extra type info */
      }
      else {  /* nvars > nexps */
        int filledtype = filledwithnil ? LUA_TNIL : LUA_TNONE;
        for (; nexps < nvars; nexps++)
          pushpaddedtyping(ls->fs, filledtype);
      }
    }
    else {
      luaK_setoneret(ls->fs, &e);  /* close last expression */
      luaK_storevar(ls->fs, &lh->v, &e);
      return;  /* avoid default */
    }
  }
  init_exp(&e, VNONRELOC, ls->fs->freereg-1);  /* default assignment */
  popexptyping(ls->fs, &e);
  luaK_storevar(ls->fs, &lh->v, &e);
  if (nvars == 1)
    popexplistentry(ls->fs);
}


static int cond (LexState *ls) {
  /* cond -> exp */
  expdesc v;
  expr(ls, &v);  /* read condition */
  if (v.k == VNIL) v.k = VFALSE;  /* `falses' are all equal here */
  luaK_goiftrue(ls->fs, &v);
  return v.f;
}


static void breakstat (LexState *ls) {
  FuncState *fs = ls->fs;
  BlockCnt *bl = fs->bl;
  int upval = 0;
  while (bl && !bl->isbreakable) {
    upval |= bl->upval;
    bl = bl->previous;
  }
  if (!bl)
    luaX_inputerror(ls, "Break instruction not allowed: no enclosing loop to "
                    "break.");
  if (upval)
    luaK_codeABC(fs, OP_CLOSE, bl->nactvar, 0, 0);
  luaK_concat(fs, &bl->breaklist, luaK_jump(fs));
}


static void whilestat (LexState *ls, int line) {
  /* whilestat -> WHILE cond DO block END */
  FuncState *fs = ls->fs;
  int whileinit;
  int condexit;
  BlockCnt bl;
  luaX_next(ls);  /* skip WHILE */
  whileinit = luaK_getlabel(fs);
  condexit = cond(ls);
  enterblock(fs, &bl, 1);
  checknext(ls, TK_DO);
  block(ls);
  luaK_patchlist(fs, luaK_jump(fs), whileinit);
#ifndef HKSC_TEST_WITH_STANDARD_LUA
  luaK_fixline(fs, line);
#endif /* HKSC_TEST_WITH_STANDARD_LUA */
  check_match(ls, TK_END, TK_WHILE, line);
  leaveblock(fs);
  luaK_patchtohere(fs, condexit);  /* false conditions finish the loop */
}


static void repeatstat (LexState *ls, int line) {
  /* repeatstat -> REPEAT block UNTIL cond */
  int condexit;
  FuncState *fs = ls->fs;
  int repeat_init = luaK_getlabel(fs);
  BlockCnt bl1, bl2;
  enterblock(fs, &bl1, 1);  /* loop block */
  enterblock(fs, &bl2, 0);  /* scope block */
  luaX_next(ls);  /* skip REPEAT */
  chunk(ls);
  check_match(ls, TK_UNTIL, TK_REPEAT, line);
  condexit = cond(ls);  /* read condition (inside scope block) */
  if (!bl2.upval) {  /* no upvalues? */
    leaveblock(fs);  /* finish scope */
    luaK_patchlist(ls->fs, condexit, repeat_init);  /* close the loop */
  }
  else {  /* complete semantics when there are upvalues */
    breakstat(ls);  /* if condition then break */
    luaK_patchtohere(ls->fs, condexit);  /* else... */
    leaveblock(fs);  /* finish scope... */
    luaK_patchlist(ls->fs, luaK_jump(fs), repeat_init);  /* and repeat */
  }
  leaveblock(fs);  /* finish loop */
}


static int exp1 (LexState *ls) {
  expdesc e;
  int k;
  expr(ls, &e);
  k = e.k;
  luaK_exp2nextreg(ls->fs, &e);
  return k;
}


#if HKSC_STRUCTURE_EXTENSION_ON
static void forbodychecktype (FuncState *fs, int nvars) {
  int i;
  lua_assert(fs->nactvar >= nvars);
  for (i = 0; i < nvars; i++) {
    int reg = fs->nactvar-i-1;
    TypeInfo *t = &fs->a->locvarstyping[reg];
    luaK_checktype(fs, t, reg);
  }
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


static void forbody (LexState *ls, int base, int line, int nvars, int isnum) {
  /* forbody -> DO block */
  BlockCnt bl;
  FuncState *fs = ls->fs;
  int prep, endfor;
  adjustlocalvars(ls, 3);  /* control variables */
  checknext(ls, TK_DO);
  prep = isnum ? luaK_codeAsBx(fs, OP_FORPREP, base, NO_JUMP) : luaK_jump(fs);
  enterblock(fs, &bl, 0);  /* scope for declared variables */
  adjustlocalvars(ls, nvars);
  luaK_reserveregs(fs, nvars);
#if HKSC_STRUCTURE_EXTENSION_ON
  forbodychecktype(fs, nvars);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  block(ls);
  leaveblock(fs);  /* end of scope for declared variables */
  luaK_patchtohere(fs, prep);
  endfor = (isnum) ? luaK_codeAsBx(fs, OP_FORLOOP, base, NO_JUMP) :
                     luaK_codeABC(fs, OP_TFORLOOP, base, 0, nvars);
  luaK_fixline(fs, line);  /* pretend that `OP_FOR' starts the loop */
  luaK_patchlist(fs, (isnum ? endfor : luaK_jump(fs)), prep + 1);
}


struct ParameterDecl {
  TString *varname;
#if HKSC_STRUCTURE_EXTENSION_ON
  TString *typename;  /* the type, or NULL if not typed */
#endif
};


static void fornum (LexState *ls, struct ParameterDecl decl, int line) {
  /* fornum -> NAME = exp1,exp1[,exp1] forbody */
  FuncState *fs = ls->fs;
  int base = fs->freereg;
  new_typedlocalvarliteral(ls, "(for index)", 0, LUA_TNUMBER, NULL);
  new_typedlocalvarliteral(ls, "(for limit)", 1, LUA_TNUMBER, NULL);
  new_typedlocalvarliteral(ls, "(for step)", 2, LUA_TNUMBER, NULL);
#if HKSC_STRUCTURE_EXTENSION_ON
  if (decl.typename != NULL) {
    TypeInfo t;
    /* parse the type of the declaration; only "number" is allowed */
    parsetype(ls, &t, decl.typename);
    if (t.type != LUA_TNONE && t.type != LUA_TNUMBER) {
      const char *msg = luaO_pushfstring(ls->H, "Only 'number' is allowed as a"
            " type for numeric FOR iterator. Got '%s'", getstr(decl.typename));
      luaX_syntaxerror(ls, msg);
    }
    new_typedlocalvar(ls, decl.varname, 3, t.type, NULL);
  }
  else
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  new_localvar(ls, decl.varname, 3);
  checknext(ls, '=');
  exp1(ls);  /* initial value */
  checknext(ls, ',');
  exp1(ls);  /* limit */
  if (testnext(ls, ','))
    exp1(ls);  /* optional step */
  else {  /* default step = 1 */
    luaK_codeABx(fs, OP_LOADK, fs->freereg, luaK_numberK(fs, 1));
    luaK_reserveregs(fs, 1);
  }
  forbody(ls, base, line, 1, 1);
}


static void forlist (LexState *ls, struct ParameterDecl decl) {
  /* forlist -> NAME {,NAME} IN explist1 forbody */
  TString *indexname = decl.varname;
  FuncState *fs = ls->fs;
  expdesc e;
  int nvars = 0;
  int line;
  int base = fs->freereg;
  /* create control variables */
  new_localvarliteral(ls, "(for generator)", nvars++);
  new_localvarliteral(ls, "(for state)", nvars++);
  new_localvarliteral(ls, "(for control)", nvars++);
  /* create declared variables */
#if HKSC_STRUCTURE_EXTENSION_ON
  if (decl.typename != NULL)
    typedlocvar(ls, indexname, decl.typename, nvars++);
  else
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  new_localvar(ls, indexname, nvars++);
  while (testnext(ls, ',')) {
    /* I parse it in this order so that `near' token in the error message
       matches exactly with Havok Script when structures are disabled */
    TString *varname = str_peekname(ls);
    luaX_lookahead(ls);  /* lookahead for `:' */
    if (ls->lookahead.token == ':') {
#if HKSC_STRUCTURE_EXTENSION_ON
      luaX_next(ls);  /* skip name */
      luaX_next(ls);  /* skip `:' */
      typedlocvar(ls, varname, str_checkname(ls), nvars);
#else /* !HKSC_STRUCTURE_EXTENSION_ON */
      error_typedlocalvar(ls);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    }
    else {
      luaX_next(ls);  /* advance over name */
      new_localvar(ls, varname, nvars);
    }
    nvars++;
  }
  checknext(ls, TK_IN);
  line = ls->linenumber;
  pushexplistentry(fs);
  adjust_assign(ls, 3, explist1(ls, &e), &e);
  popexplistentry(fs);
  luaK_checkstack(fs, 3);  /* extra space to call generator */
  forbody(ls, base, line, nvars - 3, 0);
}


static void forstat (LexState *ls, int line) {
  /* forstat -> FOR (fornum | forlist) END */
  FuncState *fs = ls->fs;
  struct ParameterDecl decl = {NULL};
  BlockCnt bl;
  enterblock(fs, &bl, 1);  /* scope for loop and control variables */
  luaX_next(ls);  /* skip `for' */
  decl.varname = str_peekname(ls);
  luaX_lookahead(ls);
  if (ls->lookahead.token == ':') {
#if HKSC_STRUCTURE_EXTENSION_ON
    luaX_next(ls);  /* skip name */
    luaX_next(ls);  /* skip `:' */
    decl.typename = str_checkname(ls);
#else /* !HKSC_STRUCTURE_EXTENSION_ON */
    error_typedlocalvar(ls);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  }
  else luaX_next(ls);  /* advance over first variable name */
  switch (ls->t.token) {
    case '=': fornum(ls, decl, line); break;
    case ',': case TK_IN: forlist(ls, decl); break;
    default: luaX_syntaxerror(ls, LUA_QL("=") " or " LUA_QL("in") " expected");
  }
  check_match(ls, TK_END, TK_FOR, line);
  leaveblock(fs);  /* loop scope (`break' jumps to this point) */
}


static int test_then_block (LexState *ls) {
  /* test_then_block -> [IF | ELSEIF] cond THEN block */
  int condexit;
  luaX_next(ls);  /* skip IF or ELSEIF */
  condexit = cond(ls);
  checknext(ls, TK_THEN);
  block(ls);  /* `then' part */
  return condexit;
}


static void ifstat (LexState *ls, int line) {
  /* ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END */
  FuncState *fs = ls->fs;
  int flist;
  int escapelist = NO_JUMP;
  flist = test_then_block(ls);  /* IF cond THEN block */
  while (ls->t.token == TK_ELSEIF) {
    luaK_concat(fs, &escapelist, luaK_jump(fs));
    luaK_patchtohere(fs, flist);
    flist = test_then_block(ls);  /* ELSEIF cond THEN block */
  }
  if (ls->t.token == TK_ELSE) {
    luaK_concat(fs, &escapelist, luaK_jump(fs));
    luaK_patchtohere(fs, flist);
    luaX_next(ls);  /* skip ELSE (after patch, for correct line info) */
    block(ls);  /* `else' part */
  }
  else
    luaK_concat(fs, &escapelist, flist);
  luaK_patchtohere(fs, escapelist);
  check_match(ls, TK_END, TK_IF, line);
}


static void localfunc (LexState *ls) {
  expdesc v, b;
  FuncState *fs = ls->fs;
  TString *name = str_checkname(ls);
  new_localvar(ls, name, 0);
  addnamepart(ls, name, NAMEPART_NAME); /* add the name part to the chain */
  init_typed_exp(&v, VLOCAL, fs->freereg, LUA_TIFUNCTION);
  luaK_reserveregs(fs, 1);
  adjustlocalvars(ls, 1);
  body(ls, &b, 0, ls->linenumber);
  luaK_storevar(fs, &v, &b);
  /* debug information will only see the variable after this point! */
  getlocvar(fs, fs->nactvar - 1).startpc = fs->pc;
}


static void localstat (LexState *ls) {
  /* stat -> LOCAL NAME {`,' NAME} [`=' explist1] */
  FuncState *fs = ls->fs;
  int nvars = 0;
  int nexps;
  int seentypedvar = 0;
  expdesc e;
  (void)seentypedvar; /* may not be used */
  lua_assert(ls->fs->nlocalslhs == 0);
  do {
    TypeInfo t;
    TString *varname = str_peekname(ls);
    luaX_lookahead(ls);
    if (ls->lookahead.token == ':') {  /* LOCAL NAME : TYPE */
#if HKSC_STRUCTURE_EXTENSION_ON
      luaX_next(ls);  /* skip variable name */
      luaX_next(ls);  /* skip `:' */
      /* the type name is advanced over adter parsing the type info to match
         the `near' token in error messages exactly with Havok Script */
      parsetype(ls, &t, str_peekname(ls));
      luaX_next(ls);  /* advance over type name */
      new_typedlocalvar(ls, varname, nvars, t.type, t.proto);
#else /* !HKSC_STRUCTURE_EXTENSION_ON */
      error_typedlocalvar(ls);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    }
    else {
      luaX_next(ls);  /* advance over name */
      new_localvar(ls, varname, nvars);
      t.type = LUA_TNONE; t.proto = NULL; t.is_static = 0;
    }
    seentypedvar |= (t.is_static != 0);
    pushlhstyping(ls, &t);
    nvars++;
  } while (testnext(ls, ','));
  if (testnext(ls, '=')) {
    pushexplistentry(fs);
#if HKSC_STRUCTURE_EXTENSION_ON
    if (seentypedvar) {
      ExpListEntry *l = gettopexplistentry(fs);
      int i;
      for (i = fs->nlocalslhs-1; i >= 0; i--)
        pushtypeconstraint(fs, l, &fs->a->lhstyping[i]);
      l->hasconstraints = 1;
    }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
    nexps = explist1(ls, &e);
  }
  else {
    e.k = VVOID;
    nexps = 0;
  }
  adjust_assign(ls, nvars, nexps, &e);
  adjustlocalvars(ls, nvars);
  fs->nlocalslhs = 0;
  if (nexps)
    popexplistentry(fs);
}


static int funcname (LexState *ls, expdesc *v) {
  /* funcname -> NAME {field} [`:' NAME] */
  int needself = 0;
  TString *name = singlevar(ls, v);
  addnamepart(ls, name, NAMEPART_NAME); /* add the name part to the chain */
  while (ls->t.token == '.')
    field(ls, v, NAMEPART_FIELD);
  if (ls->t.token == ':') {
    needself = 1;
    field(ls, v, NAMEPART_SELF);
  }
  return needself;
}


static void funcstat (LexState *ls, int line) {
  /* funcstat -> FUNCTION funcname body */
  int needself;
  expdesc v, b;
  luaX_next(ls);  /* skip FUNCTION */
  needself = funcname(ls, &v);
  body(ls, &b, needself, line);
  luaK_storevar(ls->fs, &v, &b);
  luaK_fixline(ls->fs, line);  /* definition `happens' in the first line */
}


#if HKSC_STRUCTURE_EXTENSION_ON
static void structstat (LexState *ls) {
  /* structstat -> HSTRUCTURE name structbody */
  lu_byte seenregularslot = 0;
  TString *name = structname(ls);
  initstruct(ls, name);
  /* parse the struct body */
  while (ls->t.token != TK_END) {
    TString *slotname, *slottype;
    if (testnext(ls, ';'))
      continue;
    slotname = str_checkname(ls);
    if (strcmp(getstr(slotname), "proxytable") == 0) {
      if (seenregularslot) {
        badslotorder:
        luaX_semerror(ls,
            "proxytable/meta definition must be at the start of the struct.");
      }
      addstructproxy(ls);
    }
    else if (strcmp(getstr(slotname), "meta") == 0) {
      if (seenregularslot)
        goto badslotorder;
      checknext(ls, ':');
      slottype = str_peekname(ls);
      addstructmeta(ls, slottype);
      luaX_next(ls);
    }
    else {
      seenregularslot = 1;
      checknext(ls, ':');
      slottype = str_peekname(ls);
      addstructslot(ls, slotname, slottype);
      luaX_next(ls);
    }
  }
  finalizestruct(ls);  /* add the struct to the Lua state */
  luaX_next(ls);  /* skip END */
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


static void exprstat (LexState *ls) {
  /* stat -> func | assignment */
  FuncState *fs = ls->fs;
  struct LHS_assign v;
  primaryexp(ls, &v.v);
  if (v.v.k == VCALL)  /* stat -> func */
    SETARG_C(getcode(fs, &v.v), 1);  /* call statement uses no results */
  else {  /* stat -> assignment */
    v.prev = NULL;
    assignment(ls, &v, 1);
  }
}


#ifdef LUA_CODIW6
static void deletestat (LexState *ls) {
  /* stat -> HDELETE `(' expression `)' */
  FuncState *fs = ls->fs;
  expdesc v;
  int line;
  checknext(ls, '(');
  line = ls->linenumber;
  primaryexp(ls, &v);
  if (v.k == VCALL)
    luaX_syntaxerror(ls, "Delete must be given assignable arguments");
  check_condition(ls, VLOCAL <= v.k && v.k <= VINDEXED,
                      "syntax error (non-variable in a delete expression)");
  check_match(ls, ')', '(', line);
  luaK_delete(fs, &v);
}
#endif /* LUA_CODIW6 */


static void retstat (LexState *ls) {
  /* stat -> RETURN explist */
  FuncState *fs = ls->fs;
  expdesc e;
  int first, nret;  /* registers with returned values */
  luaX_next(ls);  /* skip RETURN */
  if (block_follow(ls->t.token) || ls->t.token == ';')
    first = nret = 0;  /* return no values */
  else {
    nret = explist1_notyping(ls, &e);  /* optional return values */
    if (hasmultret(e.k)) {
      luaK_setmultret(fs, &e);
      if (e.k == VCALL && nret == 1) {  /* tail call? */
        SET_OPCODE(getcode(fs,&e), OP_TAILCALL);
        lua_assert(GETARG_A(getcode(fs,&e)) == fs->nactvar);
      }
      first = fs->nactvar;
      nret = LUA_MULTRET;  /* return all values */
    }
    else {
      if (nret == 1)  /* only one single value? */
        first = luaK_exp2anyreg(fs, &e);
      else {
        luaK_exp2nextreg(fs, &e);  /* values must go to the `stack' */
        first = fs->nactvar;  /* return all `active' values */
        lua_assert(nret == fs->freereg - first);
      }
    }
  }
  luaK_ret(fs, first, nret);
}


static int statement (LexState *ls) {
  int line = ls->linenumber;  /* may be needed for error messages */
  switch (ls->t.token) {
    case TK_IF: {  /* stat -> ifstat */
      ifstat(ls, line);
      return 0;
    }
    case TK_WHILE: {  /* stat -> whilestat */
      whilestat(ls, line);
      return 0;
    }
    case TK_DO: {  /* stat -> DO block END */
      luaX_next(ls);  /* skip DO */
      block(ls);
      check_match(ls, TK_END, TK_DO, line);
      return 0;
    }
    case TK_FOR: {  /* stat -> forstat */
      forstat(ls, line);
      return 0;
    }
    case TK_REPEAT: {  /* stat -> repeatstat */
      repeatstat(ls, line);
      return 0;
    }
    case TK_FUNCTION: {
      funcstat(ls, line);  /* stat -> funcstat */
      return 0;
    }
    case TK_LOCAL: {  /* stat -> localstat */
      luaX_next(ls);  /* skip LOCAL */
      if (testnext(ls, TK_FUNCTION))  /* local function? */
        localfunc(ls);
      else
        localstat(ls);
      return 0;
    }
    case TK_RETURN: {  /* stat -> retstat */
      retstat(ls);
      return 1;  /* must be last statement */
    }
    case TK_BREAK: {  /* stat -> breakstat */
      luaX_next(ls);  /* skip BREAK */
      breakstat(ls);
      return 1;  /* must be last statement */
    }
#if HKSC_STRUCTURE_EXTENSION_ON
    case TK_STRUCT: {
      luaX_next(ls);  /* skip HSTRUCTURE */
      structstat(ls);
      return 0;
    }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
#ifdef LUA_CODIW6
    case TK_DELETE: {  /* stat -> deletestat */
      luaX_next(ls);  /* skip HDELETE */
      deletestat(ls);
      return 0;
    }
#endif /* LUA_CODIW6 */
    default: {
      exprstat(ls);
      return 0;  /* to avoid warnings */
    }
  }
}


static void chunk (LexState *ls) {
  /* chunk -> { stat [`;'] } */
  int islast = 0;
  enterlevel(ls);
  while (!islast && !block_follow(ls->t.token)) {
    islast = statement(ls);
    testnext(ls, ';');
    lua_assert(ls->fs->f->maxstacksize >= ls->fs->freereg &&
               ls->fs->freereg >= ls->fs->nactvar);
    ls->fs->freereg = ls->fs->nactvar;  /* free registers */
  }
  leavelevel(ls);
}

/* }====================================================================== */
