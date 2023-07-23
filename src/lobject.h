/*
** $Id: lobject.h $
** Type definitions for Lua objects
** See Copyright Notice in lua.h
*/


#ifndef lobject_h
#define lobject_h


#include <stdarg.h>


#include "llimits.h"
#include "hksclua.h"


/* tags for values visible from Lua */
#define LAST_TAG  (LUA_NUM_TYPE_OBJECTS-1)

#define NUM_TAGS  (LAST_TAG+1)


/*
** Extra tags for non-values
*/
#define LUA_TPROTO  (LAST_TAG+1)
#define LUA_TUPVAL  (LAST_TAG+2)
#define LUA_TDEADKEY  (LAST_TAG+3)
#define LUA_TANALYZER  (LAST_TAG+4)
#define LUA_TTYPEANALYZER  (LAST_TAG+5)


/*
** Union of all collectable objects
*/
typedef union GCObject GCObject;


/*
** Common Header for all collectable objects (in macro form, to be
** included in other objects)
*/
#define CommonHeader  GCObject *next; lu_byte tt; lu_byte marked


/*
** Common header in struct form
*/
typedef struct GCheader {
  CommonHeader;
} GCheader;


#ifdef LUA_UI64_S
struct lua_ui64_s { /* typedef'd as lu_int64 */
  lu_int32 hi; /* bits 33-64 */
  lu_int32 lo; /* bits 1-32 */
};
#endif /* LUA_UI64_S */


/*
** Union of all Lua values
*/
typedef union {
  GCObject *gc;
  void *p;
  lua_Number n;
  lu_int64 l;
  int b;
} Value;


/*
** Tagged Values
*/

#define TValuefields  Value value; int tt

typedef struct lua_TValue {
  TValuefields;
} TValue;


/* Macros to test type */
#define ttisnil(o)  (ttype(o) == LUA_TNIL)
#define ttisnumber(o)  (ttype(o) == LUA_TNUMBER)
#define ttisstring(o)  (ttype(o) == LUA_TSTRING)
#define ttistable(o)  (ttype(o) == LUA_TTABLE)
#define ttisfunction(o)  (ttype(o) == LUA_TFUNCTION)
#define ttisboolean(o)  (ttype(o) == LUA_TBOOLEAN)
#define ttisuserdata(o)  (ttype(o) == LUA_TUSERDATA)
#define ttisthread(o)  (ttype(o) == LUA_TTHREAD)
#define ttislightuserdata(o)  (ttype(o) == LUA_TLIGHTUSERDATA)
#define ttisui64(o)  (ttype(o) == LUA_TUI64)
#define ttisstruct(o)  (ttype(o) == LUA_TSTRUCT)
#define ttisifunction(o)  (ttype(o) == LUA_TIFUNCTION)
#define ttiscfunction(o)  (ttype(o) == LUA_TCFUNCTION)

/* Macros to access values */
#define ttype(o)  ((o)->tt)
#define gcvalue(o)  check_exp(iscollectable(o), (o)->value.gc)
#define pvalue(o)  check_exp(ttislightuserdata(o), (o)->value.p)
#define nvalue(o)  check_exp(ttisnumber(o), (o)->value.n)
#define ui64value(o)  check_exp(ttisui64(o), (o)->value.l)
#define rawtsvalue(o)  check_exp(ttisstring(o), &(o)->value.gc->ts)
#define tsvalue(o)  (&rawtsvalue(o)->tsv)
#define rawuvalue(o)  check_exp(ttisuserdata(o), &(o)->value.gc->u)
#define uvalue(o)  (&rawuvalue(o)->uv)
#define clvalue(o)  check_exp(ttisfunction(o), &(o)->value.gc->cl)
#define hvalue(o)  check_exp(ttistable(o), &(o)->value.gc->h)
#define bvalue(o)  check_exp(ttisboolean(o), (o)->value.b)
#define thvalue(o)  check_exp(ttisthread(o), &(o)->value.gc->th)

#define l_isfalse(o)  (ttisnil(o) || (ttisboolean(o) && bvalue(o) == 0))

/*
** for internal debug only
*/
#define checkconsistency(obj) \
  lua_assert(!iscollectable(obj) || (ttype(obj) == (obj)->value.gc->gch.tt))

#define checkliveness(g,obj) \
  lua_assert(!iscollectable(obj) || \
  ((ttype(obj) == (obj)->value.gc->gch.tt) && !isdead(g, (obj)->value.gc)))


/* Macros to set values */
#define setnilvalue(obj) do { (obj)->tt=LUA_TNIL; } while (0)

#define setnvalue(obj,x) do \
  { TValue *i_o=(obj); i_o->value.n=(x); i_o->tt=LUA_TNUMBER; } while(0)

#define setpvalue(obj,x) do \
  { TValue *i_o=(obj); i_o->value.p=(x); i_o->tt=LUA_TLIGHTUSERDATA; } while(0)

#define setbvalue(obj,x) do \
  { TValue *i_o=(obj); i_o->value.b=(x); i_o->tt=LUA_TBOOLEAN; } while(0)

#define setsvalue(obj,x) do \
  { TValue *i_o=(obj); \
    i_o->value.gc=cast(GCObject *, (x)); i_o->tt=LUA_TSTRING; } while(0)

#define setuvalue(obj,x) do \
  { TValue *i_o=(obj); \
    i_o->value.gc=cast(GCObject *, (x)); i_o->tt=LUA_TUSERDATA; } while(0)

#define setthvalue(obj,x) do \
  { TValue *i_o=(obj); \
    i_o->value.gc=cast(GCObject *, (x)); i_o->tt=LUA_TTHREAD; } while (0)

#define setclvalue(obj,x) do \
  { TValue *i_o=(obj); \
    i_o->value.gc=cast(GCObject *, (x)); i_o->tt=LUA_TFUNCTION; } while (0)

#define sethvalue(obj,x) do \
  { TValue *i_o=(obj); \
    i_o->value.gc=cast(GCObject *, (x)); i_o->tt=LUA_TTABLE; } while (0)

#define setptvalue(obj,x) do \
  { TValue *i_o=(obj); \
    i_o->value.gc=cast(GCObject *, (x)); i_o->tt=LUA_TPROTO; } while (0)

#define setui64value(obj,x) do \
  { TValue *i_o=(obj); i_o->value.l=(x); i_o->tt=LUA_TUI64; } while (0)




#define setobj(obj1,obj2) \
  { const TValue *o2=(obj2); TValue *o1=(obj1); \
    o1->value = o2->value; o1->tt=o2->tt; }


/* convert an integral type to and from a pointer */
#define i2pvalue(i,t)  cast(void *, cast(size_t, cast(unsigned t, i)))
#define pvalue2i(p,t)  cast(t, cast(size_t, p))

/*
** different types of sets, according to destination
*/

/* from stack to (same) stack */
#define setobjs2s  setobj
/* to stack (not from same stack) */
#define setobj2s  setobj
#define setsvalue2s  setsvalue
#define sethvalue2s  sethvalue
#define setptvalue2s  setptvalue
/* from table to same table */
#define setobjt2t  setobj
/* to table */
#define setobj2t  setobj
/* to new object */
#define setobj2n  setobj
#define setsvalue2n  setsvalue

#define setttype(obj, tt) (ttype(obj) = (tt))


#define iscollectable(o)  (ttype(o) >= LUA_TSTRING && !ttisui64(o))



typedef TValue *StkId;  /* index to stack elements */


/*
** String headers for string table
*/
typedef union TString {
  L_Umaxalign dummy;  /* ensures maximum alignment for strings */
  struct {
    CommonHeader;
    lu_byte reserved;
    unsigned int hash;
    size_t len;
  } tsv;
} TString;


#define getstr(ts)  cast(const char *, (ts) + 1)
#define svalue(o)       getstr(rawtsvalue(o))



typedef union Udata {
  L_Umaxalign dummy;  /* ensures maximum alignment for `local' udata */
  struct {
    CommonHeader;
    struct Table *metatable;
    struct Table *env;
    size_t len;
  } uv;
} Udata;




/*
** Function Prototypes
*/
typedef struct Proto {
  CommonHeader;
  TValue *k;  /* constants used by the function */
  Instruction *code;
  struct Proto **p;  /* functions defined inside the function */
  int *lineinfo;  /* map from opcodes to source lines */
  struct LocVar *locvars;  /* information about local variables */
  TString **upvalues;  /* upvalue names */
  TString  *source;
  TString  *name;
#ifdef LUA_CODT6
  lu_int32 hash;
#endif /* LUA_CODT6 */
  int sizeupvalues;
  int sizek;  /* size of `k' */
  int sizecode;
  int sizelineinfo;
  int sizep;  /* size of `p' */
  int sizelocvars;
  int linedefined;
  int lastlinedefined;
  GCObject *gclist;
  lu_byte nups;  /* number of upvalues */
  lu_byte numparams;
  lu_byte is_vararg;
  lu_byte maxstacksize;
} Proto;


/* masks for new-style vararg */
#define VARARG_HASARG    1
#define VARARG_ISVARARG    2
#define VARARG_NEEDSARG    4


typedef struct LocVar {
  TString *varname;
  int startpc;  /* first point where variable is active */
  int endpc;    /* first point where variable is dead */
} LocVar;



#ifdef HKSC_DECOMPILER

/* flags defined in lanalyzer.h */
typedef lu_int32 InstructionFlags;
struct BlockState;
struct SlotDesc;
struct BlockNode;
struct ExpNode;
struct OpenExpr;
struct RegNote;

/*
** Function analyzers
*/
typedef struct Analyzer {
  CommonHeader;
  InstructionFlags *insproperties;  /* instruction flags */
  struct OpenExpr *opencalls;
  struct RegNote *regnotes;
  struct SlotDesc *regproperties;  /* register properties */
  struct LocVar *locvars;  /* information about local variables */
  TString **upvalues;  /* upvalue names */
  lu_int32 *kmap;  /* bitmap blocks for marking referenced constants */
  unsigned short *actvar;
  int sizeinsproperties;
  int sizeopencalls;
  int sizeregnotes;
  int sizeregproperties;
  int sizelocvars;
  int sizeactvar;
  int sizeupvalues;
  int sizekmap;
  int decomppass;  /* which decompiler pass */
  /* when the function has <= 32 constants, KMAP points to this field */
  lu_int32 kmap_1;
  struct {
    struct BlockNode *first, *last;
  } bllist;
  struct {
    /* the first pass keeps a stack of pending block states, the second pass
       keeps a stack of pending expression nodes; both stacks only live within
       their respective decompiler passes */
    union { struct BlockState *s1; struct ExpNode *s2; } u;
    int total;  /* number of allocated elements in the stack */
    int used;  /* number of used elements in the stack */
  } pendingstk;
} Analyzer;

#endif /* HKSC_DECOMPILER */


struct StructProto;


#if HKSC_STRUCTURE_EXTENSION_ON

/*
** Structure Prototypes
*/
typedef struct StructProto {
  size_t nslots;  /* number of slots in this structure definition */
  lu_byte hasmeta;  /* true if a meta slot has been defined */
  lu_byte hasproxy;  /* true if a proxy table slot has been defined */
  short id;  /* unique structure id */
  TString *name;  /* structure name */
} StructProto;


/* the maximum valid struct prototype id */
#define LAST_STRUCT_ID ((LUAI_MAXSTRUCTS)-1)


#define getproto(u)  cast(StructProto *, (u)+1)
#define getprotoslots(p)  cast(StructSlot *, (p)+1)
#define sizestruct(n) (sizeof(StructProto) + (n) * sizeof(StructSlot))

#define id2pvalue(id)  i2pvalue(id, short)
#define pvalue2id(o)  pvalue2i(pvalue(o), short)

/*
** reserved slot types
*/
#define SLOT_RESERVED_NONE  0  /* not a reserved slot */
#define SLOT_RESERVED_META  1  /* meta slot */
#define SLOT_RESERVED_PROXY  2  /* proxy table slot */
#define SLOT_RESERVED_INTERNAL  3  /* struct prototype slot */
#define NUM_SLOTS_RESERVED  3

#define SLOT_INDEX_INTERNAL  0
#define SLOT_INDEX_META  1
#define SLOT_INDEX_PROXY  2

#define getmetaslot(p)  getprotoslots(p)+SLOT_INDEX_META
#define getproxytableslot(p)  getprotoslots(p)+SLOT_INDEX_PROXY

/* maximum number of slot definitions allowed per structure prototype */
#define MAX_STRUCT_SLOTS  224

/*
** Structure Slot Descriptors
*/
typedef struct StructSlot {
  TString *name;  /* slot name */
  short structid;  /* structure id if this slot is a structure type */
  lu_byte typeid;  /* Lua type id of this slot */
  lu_byte reserved;  /* if a reserved slot, a non-zero id, otherwise, zero */
  lu_byte index;  /* slot index in its parent structure prototype */
  lu_byte position;  /* slot position according to struct layout */
} StructSlot;


#endif /* HKSC_STRUCTURE_EXTENSION_ON */


#define typeinfoinit(t,p,s) {(p), (t), (s)}

typedef struct TypeInfo {
  struct StructProto *proto;
  int type;
  lu_byte is_static;
} TypeInfo;


typedef struct ExpListEntry {
  TypeInfo *types;
  TypeInfo *constraints;
  lu_byte hasconstraints;
  int sizetypes;
  int sizeconstraints;
  int ntypes;
  int nconstraints;
} ExpListEntry;


/*
** allocated data used by a compiler per-function
*/
typedef struct TypeAnalyzer {
  CommonHeader;
  TypeInfo *locvarstyping;  /* local variables typing */
  TypeInfo *lhstyping;  /* LHS assignment typing */
  int sizelocvarstyping;
  int sizelhstyping;
  ExpListEntry *explists;  /* expression list stack */
  int sizeexplists;
  int nexplists;
} TypeAnalyzer;



/*
** Upvalues
*/

typedef struct UpVal {
  CommonHeader;
  TValue *v;  /* points to stack or to its own value */
  union {
    TValue value;  /* the value (when closed) */
    struct {  /* double linked list (when open) */
      struct UpVal *prev;
      struct UpVal *next;
    } l;
  } u;
} UpVal;


/*
** Closures
*/

#define ClosureHeader \
  CommonHeader; lu_byte isC; lu_byte nupvalues; GCObject *gclist; \
  struct Table *env

typedef struct CClosure {
  ClosureHeader;
  lua_CFunction f;
  TValue upvalue[1];
} CClosure;


typedef struct LClosure {
  ClosureHeader;
  struct Proto *p;
  UpVal *upvals[1];
} LClosure;


typedef union Closure {
  CClosure c;
  LClosure l;
} Closure;


#define iscfunction(o)  (ttiscfunction(o))
#define isLfunction(o)  (ttisifunction(o))


/*
** Tables
*/

typedef union TKey {
  struct {
    TValuefields;
    struct Node *next;  /* for chaining */
#ifdef HKSC_FROMSOFT_TTABLES
    /* linked list of new key insertions */
    struct Node *previnserted, *nextinserted;
#endif
  } nk;
  TValue tvk;
} TKey;


typedef struct Node {
  TValue i_val;
  TKey i_key;
} Node;


typedef struct Table {
  CommonHeader;
  lu_byte flags;  /* 1<<p means tagmethod(p) is not present */ 
  lu_byte lsizenode;  /* log2 of size of `node' array */
  struct Table *metatable;
  TValue *array;  /* array part */
  Node *node;
  Node *lastfree;  /* any free position is before this position */
#ifdef HKSC_FROMSOFT_TTABLES
  Node *lastinserted;  /* the last inerted new key */
#endif
  GCObject *gclist;
  int sizearray;  /* size of `array' array */
} Table;



/*
** `module' operation for hashing (size is always a power of 2)
*/
#define lmod(s,size) \
  (check_exp((size&(size-1))==0, (cast(int, (s) & ((size)-1)))))


#define twoto(x)  (1<<(x))
#define sizenode(t)  (twoto((t)->lsizenode))


#define luaO_nilobject    (&luaO_nilobject_)

LUAI_DATA const TValue luaO_nilobject_;

#define ceillog2(x)  (luaO_log2((x)-1) + 1)

LUAI_FUNC int luaO_log2 (unsigned int x);
LUAI_FUNC int luaO_int2fb (unsigned int x);
LUAI_FUNC int luaO_fb2int (int x);
LUAI_FUNC int luaO_rawequalObj (const TValue *t1, const TValue *t2);
LUAI_FUNC int luaO_str2d (const char *s, lua_Number *result);
#ifdef LUA_UI64_S /* special UI64 functions for C89 */
LUAI_FUNC struct lua_ui64_s luaO_str2ui64_s(const char *s, char **endptr,
                                          size_t n);
LUAI_FUNC int luaO_ui64_s_2str(char *str, struct lua_ui64_s literal);
#endif /* LUA_UI64_S */
LUAI_FUNC int luaO_str2ui64(const char *s,const char *suffix,lu_int64 *result);
LUAI_FUNC int luaO_ptr2str(char *str, void *p);
LUAI_FUNC TString *luaO_kstring2print (hksc_State *H, TString *ts);
LUAI_FUNC const char *luaO_pushvfstring (hksc_State *H, const char *fmt,
                                                       va_list argp);
LUAI_FUNC const char *luaO_pushfstring (hksc_State *H, const char *fmt, ...);
LUAI_FUNC void luaO_chunkid (char *out, const char *source, size_t len);

LUAI_FUNC const char *luaO_generatechunkname(hksc_State *H,
                                             const char *filename);

#endif

