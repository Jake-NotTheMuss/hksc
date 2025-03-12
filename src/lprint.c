/*
** $Id: lprint.c $
** print bytecodes
** See Copyright Notice in lua.h
*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define hksc_c
#define LUA_CORE

#include "hksclua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "llex.h"
#include "llimits.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstruct.h"
#include "lundump.h"
#include "lzio.h"

typedef struct {
  hksc_State *H;
  lua_Writer writer;
  void *data;
  const Proto *f;  /* main function */
  int status;
  int full;
  int quote;
  Mbuffer buff;
} PrintState;


#define CM "\t; " /* comment in listing */

static void PrintBlock (PrintState *P, const void *b, size_t size) {
  if (P->status == 0) {
    lua_unlock(P->H);
    P->status = (*P->writer)(P->H, b, size, P->data);
    lua_lock(P->H);
  }
}

static void PrintChar (PrintState *P, int y) {
  char x = y;
  PrintBlock(P, &x, 1);
}


static void f_pfn (const char *s, size_t l, void *ud) {
  PrintBlock(ud, s, l);
}


#define DEFCODE(name,m,t,a,b,c,mr1,ur1,vr1)  char name##_buff [sizeof(#name)];
static const int max_opcode_len = (int)(sizeof(union {
#include "lopcodes.def"
}));
#undef DEFCODE

static int Print (void *P, const char *fmt, ...) {
  va_list ap;
  int ret;
  va_start(ap, fmt);
  ret = luaO_vprintf(f_pfn, P, fmt, ap);
  va_end(ap);
  return ret;
}

typedef struct PFN {
  int (*f) (void *ud, const char *fmt, ...);
  void *ud;
} PFN;

#if HKSC_STRUCTURE_EXTENSION_ON
static void PrintStructName (hksc_State *H, PFN *pfn, short id) {
  StructProto *p = luaR_getstructbyid(H, id);
  if (p == NULL)
    (*pfn->f)(pfn->ud, "(unknown struct)");
  else
    (*pfn->f)(pfn->ud, "%s", getstr(p->name));
}

static void PrintSlotIndex (hksc_State *H, PFN *pfn, int position) {
  int slot;
  if (position == 0) return;
  slot = cast_int(luaR_pos2index(H, cast_byte(position)));
  (*pfn->f)(pfn->ud, "slot %d", slot+1);
}

#endif /* HKSC_STRUCTURE_EXTENSION_ON */

static void printk_fn (const char *s, size_t l, void *ud) {
  PFN *pfn = ud;
  (*pfn->f)(pfn->ud, "%.*s", cast_int(l), s);
}

void luaU_printcode (hksc_State *H, const Proto *f, int pc,
                     int (*pfn) (void *ud, const char *fmt, ...), void *ud,
                     int quote) {
#define Print (*pfn)
#define P ud
#define PrintK(i) luaO_printk(&f->k[i], printk_fn, &PFN, quote)
#if HKSC_STRUCTURE_EXTENSION_ON
  int tagchain = 0;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  PFN PFN;
  Instruction i=f->code[pc];
  OpCode o=GET_OPCODE(i);
  int a=GETARG_A(i);
  int b=GETARG_B(i);
  int c=GETARG_C(i);
  int bx=GETARG_Bx(i);
  int sbx=GETARG_sBx(i);
  int line=getline(f,pc);
  PFN.f = pfn;
  PFN.ud = ud;
  Print(P, "\t%d\t[", pc + 1);
  if (line > 0)
    Print(P, "%d", line);
  else
    Print(P, "-");
  Print(P, "]\t%-*s\t", max_opcode_len, luaP_opnames[o]);
  switch (getOpMode(o)) {
    case iABC:
      Print(P, "%d",a);
      if (getBMode(o)!=OpArgN) Print(P, " %d", ISK(b) ? -1 - INDEXK(b) : b);
      if (getCMode(o)!=OpArgN) Print(P, " %d", ISK(c) ? -1 - INDEXK(c) : c);
      break;
    case iABx:
      if (getBMode(o)==OpArgK) Print(P, "%d %d", a, -1 - INDEXK(bx));
      else Print(P, "%d %d", a, bx);
      break;
    case iAsBx:
      if (o==OP_JMP) Print(P, "%d",sbx); else Print(P, "%d %d",a,sbx);
      break;
  }
  switch (o) {
    case OP_LOADK:
      Print(P, CM);  PrintK(bx);
      break;
    case OP_GETUPVAL:
    case OP_SETUPVAL:
      Print(P, CM "%s", (f->sizeupvalues>0) ? getstr(f->upvalues[b]) : "-");
      break;
    case OP_GETGLOBAL: case OP_GETGLOBAL_MEM:
    case OP_SETGLOBAL:
      Print(P, CM "%s",svalue(&f->k[bx]));
      break;
    case OP_GETTABLE:
    case OP_GETTABLE_S:
    case OP_GETTABLE_N:
    case OP_SELF:
      if (ISK(c))
    case OP_GETFIELD: case OP_GETFIELD_R1:
        { Print(P, CM); PrintK(INDEXK(c)); }
      break;
    case OP_SETTABLE: case OP_SETTABLE_BK:
    case OP_SETTABLE_S: case OP_SETTABLE_S_BK:
    case OP_SETTABLE_N: case OP_SETTABLE_N_BK:
    case OP_ADD: case OP_ADD_BK:
    case OP_SUB: case OP_SUB_BK:
    case OP_MUL: case OP_MUL_BK:
    case OP_DIV: case OP_DIV_BK:
    case OP_POW: case OP_POW_BK:
    case OP_EQ:
    case OP_LT: case OP_LT_BK:
    case OP_LE: case OP_LE_BK:
#ifdef LUA_CODT7
    case OP_LEFT_SHIFT: case OP_LEFT_SHIFT_BK:
    case OP_RIGHT_SHIFT: case OP_RIGHT_SHIFT_BK:
    case OP_BIT_AND: case OP_BIT_AND_BK:
    case OP_BIT_OR: case OP_BIT_OR_BK:
#endif /* LUA_CODT7 */
      if (ISK(b) || ISK(c)) {
        Print(P, CM);
        if (ISK(b)) PrintK(INDEXK(b)); else Print(P, "-");
        Print(P, " ");
        if (ISK(c)) PrintK(INDEXK(c)); else Print(P, "-");
      }
      break;
    case OP_SETFIELD: case OP_SETFIELD_R1:
      Print(P, CM);
      PrintK(b);
      Print(P, " ");
      if (ISK(c)) PrintK(INDEXK(c)); else Print(P, "-");
      break;
    case OP_JMP:
    case OP_FORLOOP:
    case OP_FORPREP:
      Print(P, CM "to %d",sbx+pc+2);
      break;
    case OP_CLOSURE:
      Print(P, CM "%p",cast(void *, f->p[bx]));
      break;
    case OP_SETLIST:
      if (c==0) Print(P, CM "%d", GETARG_Bx(f->code[pc+1]));
      else Print(P, CM "%d",c);
      break;
#if HKSC_STRUCTURE_EXTENSION_ON
    case OP_NEWSTRUCT:
      Print(P, CM);
      PrintStructName(H, &PFN, cast(short, GETARG_Bx(f->code[pc+1])));
      break;
    case OP_SETSLOTN:
    case OP_SETSLOTI:
      Print(P, CM);
      PrintSlotIndex(H, &PFN, (o == OP_SETSLOTN) ? c : b);
      break;
    case OP_SETSLOT:
      Print(P, CM);
      PrintSlotIndex(H, &PFN, b);
      Print(P, " : ");
      Print(P, "%s", luaX_typename(GETARG_Bx(f->code[pc+1])));
      break;
    case OP_SETSLOTS:
      Print(P, CM);
      PrintSlotIndex(H, &PFN, b);
      Print(P, " : ");
      PrintStructName(H, &PFN, cast(short, GETARG_Bx(f->code[pc+1])));
      break;
    case OP_SETSLOTMT:
      tagchain = GET_SLOTMT_TAGCHAIN(i)+1;
      Print(P, CM "chain %d : ", tagchain);
      if (GET_SLOTMT_TYPE(i) == LUA_TSTRUCT)
        PrintStructName(H, &PFN, cast(short, GETARG_Bx(f->code[pc+1])));
      else
        Print(P, "%s", luaX_typename(GET_SLOTMT_TYPE(i)));
      break;
    case OP_GETSLOT:
    case OP_GETSLOT_D:
    case OP_SELFSLOT:
      Print(P, CM);
      PrintSlotIndex(H, &PFN, c);
      break;
    case OP_GETSLOTMT:
    case OP_SELFSLOTMT:
      tagchain = c+1;
      Print(P, CM "chain %d", tagchain);
      break;
    case OP_DATA:
      if (tagchain) {
        Print(P, CM);
        PrintSlotIndex(H, &PFN, a);
        if (tagchain > 1) {
          Print(P, " --> tm ");
          PrintSlotIndex(H, &PFN, bx);
        }
        tagchain--;
      }
      break;
    case OP_CHECKTYPE:
      Print(P, CM "%s", luaX_typename(bx));
      break;
    case OP_CHECKTYPES:
    case OP_CHECKTYPE_D:
      Print(P, CM);
      PrintStructName(H, &PFN, cast(short, bx));
      break;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
#ifdef LUA_CODIW6
    case OP_DELETE: case OP_DELETE_BK:
      if (c == DELETE_UPVAL)
        Print(P, CM "%s", (f->sizeupvalues>0) ? getstr(f->upvalues[b]) : "-");
      else if (c == DELETE_GLOBAL)
        Print(P, CM "%s", svalue(&f->k[INDEXK(b)]));
      else if (c == DELETE_INDEXED && ISK(b))
        Print(P, CM); PrintK(INDEXK(b));
      break;
#endif /* LUA_CODIW6 */
    default:
      break;
  }
  Print(P, "\n");
#undef Print
#undef P
#undef PrintK
  UNUSED(H);
}

static void PrintCode (PrintState *P, const Proto *f) {
  int pc;
  for (pc = 0; pc < f->sizecode; pc++)
    luaU_printcode(P->H, f, pc, Print, P, P->quote);
}

#define SS(x) (x==1)?"":"s"
#define S(x)  x,SS(x)

static void PrintHeader (PrintState *P, const Proto *f) {
  const char *s=getstr(f->source);
  const char *n=f->name ? getstr(f->name) : "(anonymous)";
  if (*s=='@' || *s=='=')
    s++;
  else if (*s==LUA_SIGNATURE[0])
    s="(bstring)";
  else
    s="(string)";
  Print(P, "\n%s <%s:%s:"
#ifdef LUA_CODT6 /* print hash */
         "%" LUA_INT_FRMLEN "x:"
#endif /* LUA_CODT6 */
         "%d,%d> (%d instruction%s, %d bytes at %p)\n",
    (f->linedefined==0)?"main":"function",s,n,
#ifdef LUA_CODT6
    f->hash,
#endif /* LUA_CODT6 */
    f->linedefined,f->lastlinedefined,
    S(f->sizecode),f->sizecode*cast_int(sizeof(Instruction)),cast(void *, f));
  Print(P, "%d%s param%s, %d slot%s, %d upvalue%s, ",
    f->numparams,f->is_vararg?"+":"",SS(f->numparams),
  S(f->maxstacksize),S(f->nups));
    Print(P, "%d local%s, %d constant%s, %d function%s\n",
  S(f->sizelocvars),S(f->sizek),S(f->sizep));
}

static void PrintConstants (PrintState *P, const Proto *f) {
  int i, n = f->sizek;
  Print(P, "constants (%d) for %p:\n",n,cast(void *, f));
  for (i = 0; i < n; i++) {
    Print(P, "\t%d\t",i+1);
    luaO_printk(&f->k[i], f_pfn, P, P->quote);
    PrintChar(P, '\n');
  }
}

static void PrintLocals (PrintState *P, const Proto *f) {
  int i, n = f->sizelocvars;
  printf("locals (%d) for %p:\n",n,cast(void *, f));
  for (i = 0; i < n; i++) {
    Print(P, "\t%d\t%s\t%d\t%d\n", i,
      getstr(f->locvars[i].varname),f->locvars[i].startpc+1,
             f->locvars[i].endpc+1);
  }
}

static void PrintUpvalues (PrintState *P, const Proto *f) {
  int i, n = f->sizeupvalues;
  Print(P, "upvalues (%d) for %p:\n",n,cast(void *, f));
  if (f->upvalues==NULL) return;
  for (i = 0; i < n; i++){
    Print(P, "\t%d\t%s\n",i,getstr(f->upvalues[i]));
  }
}


static void PrintFunction (PrintState *P, const Proto *f) {
  int i;
  PrintHeader(P, f);
  PrintCode(P, f);
  if (P->full) {
    PrintConstants(P, f);
    PrintLocals(P, f);
    PrintUpvalues(P, f);
  }
  for (i = 0; i < f->sizep; i++)
    PrintFunction(P, f->p[i]);
}


static void f_print (hksc_State *H, void *ud) {
  PrintState *P = ud;
  UNUSED(H);
  PrintFunction(P, P->f);
}


int luaU_print (hksc_State *H, const Proto *f, lua_Writer w, void *data,
                 int full) {
  PrintState P;
  int status;
  P.H = H;
  P.f = f;
  P.writer = w;
  P.data = data;
  P.full = full;
  P.quote = '"';
  P.status = 0;
  luaZ_initbuffer(H, &P.buff);
  status = luaD_pcall(H, f_print, &P);
  luaZ_freebuffer(H, &P.buff);
  if (status == 0) status = P.status;
  return status;
}
