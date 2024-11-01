/*
** $Id: lprint.c $
** print bytecodes
** See Copyright Notice in lua.h
*/

#include <ctype.h>
#include <stdio.h>
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

#define MAX_CHAR_DEC(T)  ((CHAR_BIT * sizeof(T) - 1) / 3 + 2)

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


#define PrintLiteral(P,s)  PrintBlock(P, "" s, sizeof(s)-1)
#define PrintString (P,s)  PrintBlock(P, s, strlen(s))
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


#define PrintTString(P, ts)  luaO_printstring(ts, (P)->quote, f_pfn, P)

#define DEFCODE(name,m,t,a,b,c,mr1,ur1,vr1)  char name##_buff [sizeof(#name)];
static const int max_opcode_len = (int)(sizeof(union {
#include "lopcodes.def"
}));
#undef DEFCODE

#define growbuff(H,b,n) luaZ_openspace(H, b, (b)->buffsize + n)

static void Print (PrintState *P, const char *fmt, ...) {
  va_list ap;
  hksc_State *H = P->H;
  Mbuffer *b = &P->buff;
  size_t n;
  luaZ_resetbuffer(b);
  va_start(ap, fmt);
  for (;;) {
    int advance = 2;  /* default is `%<spec>' */
    const char *e = strchr(fmt, '%');
    if (e == NULL) break;
    n = e - fmt;
    growbuff(H, b, n);
    memcpy(b->buffer + b->n, fmt, n);
    b->n += n;
    switch (*(e+1)) {
      int ival;
      unsigned int uval;
      case 'd':
        ival = va_arg(ap, int);
        printint: growbuff(H, b, MAX_CHAR_DEC(int));
        b->n += sprintf(b->buffer + b->n, "%d", ival);
        break;
      case 'u':
        uval = va_arg(ap, unsigned int);
        growbuff(H, b, MAX_CHAR_DEC(int));
        b->n += sprintf(b->buffer + b->n, "%u", uval);
        break;
      case 'c':
        ival = va_arg(ap, int);
        printch: growbuff(H, b, 1);
        b->buffer[b->n++] = ival;
        break;
      case 's': {
        const char *s = va_arg(ap, const char *);
        size_t len;
        if (s == NULL) s = "(null)";
        len = strlen(s);
        growbuff(H, b, len);
        memcpy(b->buffer + b->n, s, len);
        b->n += len;
        break;
      }
      case 'p':
        growbuff(H, b, 4*sizeof(void *) + 8);
        b->n += sprintf(b->buffer + b->n, "%p", va_arg(ap, void *));
        break;
      case '%':
        ival = '%';
        goto printch;
      case 'I': /* print pc */
        ival = va_arg(ap, int) + 1;
        goto printint;
      case 'L':  /* print line */
        ival = va_arg(ap, int);
        if (ival > 0)
          goto printint;
        ival = '-';
        goto printch;
      case 'R':  /* RK value */
        ival = va_arg(ap, int);
        if (ISK(ival)) ival = -1 - INDEXK(ival);
        goto printint;
      case 'K':  /* K value */
        ival = va_arg(ap, int);
        ival = -1 - INDEXK(ival);
        goto printint;
      case 'O': {  /* print opcode name */
        OpCode o = va_arg(ap, OpCode);
        growbuff(H, b, max_opcode_len + 2);
        b->n += sprintf(b->buffer + b->n, "%-*s", max_opcode_len,
                        luaP_opnames[o]);
        break;
      }
      default: {
        /* handle whatever the format specifier is for lu_int32 */
#ifdef LUA_CODT6
        const char hashfmt[] = "%" LUA_INT_FRMLEN "x";
        advance = sizeof(hashfmt)-1;
        if (strncmp(e, hashfmt, advance) == 0) {
          growbuff(H, b, sizeof(hashfmt));
          b->n += sprintf(b->buffer + b->n, "%" LUA_INT_FRMLEN "x",
                          va_arg(ap, lu_int32));

          break;
        }
#endif /* LUA_CODT6 */
        lua_assert(0);
      }
    }
    fmt = e + advance;
  }
  n = strlen(fmt);
  growbuff(H, b, n);
  memcpy(b->buffer + b->n, fmt, n);
  b->n += n;
  va_end(ap);
  PrintBlock(P, b->buffer, b->n);
}

static void PrintUI64 (PrintState *P, const lu_int64 literal) {
  char buff[LUAI_MAXUI642STR];
  lua_ui642str(buff, literal);
  Print(P, "0x%shl", buff);
}

static void PrintLUD (PrintState *P, size_t s) {
  char buff[MAX_CHAR_DEC(long) + 4];
  int n = sprintf(buff, "0x%lxhi", cast(unsigned long, s));
  PrintBlock(P, buff, cast(size_t, n));
}

static void PrintNumber (PrintState *P, lua_Number num) {
  char buff[LUAI_MAXNUMBER2STR];
  int n = lua_number2str(buff, num);
  PrintBlock(P, buff, cast(size_t, n));
}

static void PrintConstant (PrintState *P, const Proto *f, int i) {
  const TValue *o=&f->k[i];
  switch (ttype(o)) {
    case LUA_TNIL:
      PrintLiteral(P, "nil");
      break;
    case LUA_TBOOLEAN:
      (void)(bvalue(o) ? PrintLiteral(P, "true") : PrintLiteral(P, "false"));
      break;
    case LUA_TLIGHTUSERDATA:
      PrintLUD(P, hlvalue(o));
      break;
    case LUA_TNUMBER:
      PrintNumber(P, nvalue(o));
      break;
    case LUA_TSTRING:
      PrintTString(P, rawtsvalue(o));
      break;
    case LUA_TUI64:
      PrintUI64(P, ui64value(o));
      break;
    default:        /* cannot happen */
      Print(P, "? type=%d",ttype(o));
      break;
  }
}

#if HKSC_STRUCTURE_EXTENSION_ON
static void PrintStructName (PrintState *P, short id) {
  StructProto *p = luaR_getstructbyid(P->H, id);
  if (p == NULL)
    PrintLiteral(P, "(unknown struct)");
  else
    PrintBlock(P, getstr(p->name), p->name->tsv.len);
}

static void PrintSlotIndex (PrintState *P, int position) {
  int slot;
  if (position == 0) return;
  slot = cast_int(luaR_pos2index(P->H, cast_byte(position)));
  Print(P, "slot %d", slot+1);
}

#endif /* HKSC_STRUCTURE_EXTENSION_ON */

static void PrintCode (PrintState *P, const Proto *f) {
  int pc;
#if HKSC_STRUCTURE_EXTENSION_ON
  int tagchain = 0;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  for (pc = 0; pc < f->sizecode; pc++) {
    Instruction i=f->code[pc];
    OpCode o=GET_OPCODE(i);
    int a=GETARG_A(i);
    int b=GETARG_B(i);
    int c=GETARG_C(i);
    int bx=GETARG_Bx(i);
    int sbx=GETARG_sBx(i);
    int line=getline(f,pc);
    Print(P, "\t%I\t[%L]\t%O\t", pc, line, o);
    switch (getOpMode(o)) {
      case iABC:
        Print(P, "%d",a);
        if (getBMode(o)!=OpArgN) Print(P, " %R", b);
        if (getCMode(o)!=OpArgN) Print(P, " %R", c);
        break;
      case iABx:
        if (getBMode(o)==OpArgK) Print(P, "%d %K", a, bx);
        else Print(P, "%d %d", a, bx);
        break;
      case iAsBx:
        if (o==OP_JMP) Print(P, "%d",sbx); else Print(P, "%d %d",a,sbx);
        break;
    }
    switch (o) {
      case OP_LOADK:
        PrintLiteral(P, CM); PrintConstant(P, f, bx);
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
          { PrintLiteral(P, CM); PrintConstant(P, f,INDEXK(c)); }
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
          PrintLiteral(P, CM);
          if (ISK(b)) PrintConstant(P, f,INDEXK(b)); else PrintLiteral(P, "-");
          PrintLiteral(P, " ");
          if (ISK(c)) PrintConstant(P, f,INDEXK(c)); else PrintLiteral(P, "-");
        }
        break;
      case OP_SETFIELD: case OP_SETFIELD_R1:
        PrintLiteral(P, CM);
        PrintConstant(P,f,b);
        PrintLiteral(P, " ");
        if (ISK(c)) PrintConstant(P,f,INDEXK(c)); else PrintLiteral(P, "-");
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
        PrintLiteral(P, CM);
        PrintStructName(P, cast(short, GETARG_Bx(f->code[pc+1])));
        break;
      case OP_SETSLOTN:
      case OP_SETSLOTI:
        PrintLiteral(P, CM);
        PrintSlotIndex(P, (o == OP_SETSLOTN) ? c : b);
        break;
      case OP_SETSLOT:
        PrintLiteral(P, CM);
        PrintSlotIndex(P, b);
        PrintLiteral(P, " : ");
        Print(P, "%s", luaX_typename(GETARG_Bx(f->code[pc+1])));
        break;
      case OP_SETSLOTS:
        PrintLiteral(P, CM);
        PrintSlotIndex(P, b);
        PrintLiteral(P, " : ");
        PrintStructName(P, cast(short, GETARG_Bx(f->code[pc+1])));
        break;
      case OP_SETSLOTMT:
        tagchain = GET_SLOTMT_TAGCHAIN(i)+1;
        Print(P, CM "chain %d : ", tagchain);
        if (GET_SLOTMT_TYPE(i) == LUA_TSTRUCT)
          PrintStructName(P, cast(short, GETARG_Bx(f->code[pc+1])));
        else
          Print(P, "%s", luaX_typename(GET_SLOTMT_TYPE(i)));
        break;
      case OP_GETSLOT:
      case OP_GETSLOT_D:
      case OP_SELFSLOT:
        PrintLiteral(P, CM);
        PrintSlotIndex(P, c);
        break;
      case OP_GETSLOTMT:
      case OP_SELFSLOTMT:
        tagchain = c+1;
        Print(P, CM "chain %d", tagchain);
        break;
      case OP_DATA:
        if (tagchain) {
          PrintLiteral(P, CM);
          PrintSlotIndex(P, a);
          if (tagchain > 1) {
            PrintLiteral(P, " --> tm ");
            PrintSlotIndex(P, bx);
          }
          tagchain--;
        }
        break;
      case OP_CHECKTYPE:
        Print(P, CM "%s", luaX_typename(bx));
        break;
      case OP_CHECKTYPES:
      case OP_CHECKTYPE_D:
        PrintLiteral(P, CM);
        PrintStructName(P, cast(short, bx));
        break;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
#ifdef LUA_CODIW6
      case OP_DELETE: case OP_DELETE_BK:
        if (c == DELETE_UPVAL) {
          Print(P, CM "%s",(f->sizeupvalues>0) ? getstr(f->upvalues[b]) : "-");
        }
        else if (c == DELETE_GLOBAL) {
          Print(P, CM "%s",svalue(&f->k[INDEXK(b)]));
        }
        else if (c == DELETE_INDEXED) {
          if (ISK(b)) {
            PrintLiteral(P, CM); PrintConstant(P,f,INDEXK(b));
          }
        }
        break;
#endif /* LUA_CODIW6 */
      default:
        break;
    }
    PrintLiteral(P, "\n");
  }
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
    PrintConstant(P, f, i);
    PrintLiteral(P, "\n");
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
