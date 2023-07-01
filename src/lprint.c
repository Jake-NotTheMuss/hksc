/*
** $Id: lprint.c $
** print bytecodes
** See Copyright Notice in lua.h
*/

#include <ctype.h>
#include <stdio.h>

#define hksc_c
#define LUA_CORE

#include "hksclua.h"

#include "lcode.h"
#include "ldebug.h"
#include "llex.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstruct.h"
#include "lundump.h"

#define PrintFunction luaU_print

#define Sizeof(x)	((int)sizeof(x))
#define VOID(p)		((const void*)(p))

static void PrintString(const TString *ts)
{
  const char *s=getstr(ts);
  size_t i,n=ts->tsv.len;
  putchar('"');
  for (i=0; i<n; i++)
  {
    int c=s[i];
    switch (c)
    {
      case '"': printf("\\\""); break;
      case '\\': printf("\\\\"); break;
      case '\a': printf("\\a"); break;
      case '\b': printf("\\b"); break;
      case '\f': printf("\\f"); break;
      case '\n': printf("\\n"); break;
      case '\r': printf("\\r"); break;
      case '\t': printf("\\t"); break;
      case '\v': printf("\\v"); break;
      default:
        if (isprint((unsigned char)c))
          putchar(c);
        else
          printf("\\%03u",(unsigned char)c);
        break;
    }
  }
  putchar('"');
}

static void PrintUI64(const lu_int64 literal)
{
  char buff[LUAI_MAXUI642STR];
  lua_ui642str(buff, literal);
  printf("0x%shl", buff);
}

static void PrintLUD(void *p)
{
  char buff[LUAI_MAXUI642STR];
  luaO_ptr2str(buff, p);
  printf("0x%shi", buff);
}

static void PrintConstant(const Proto *f, int i)
{
  const TValue *o=&f->k[i];
  switch (ttype(o))
  {
    case LUA_TNIL:
      printf("nil");
      break;
    case LUA_TBOOLEAN:
      printf(bvalue(o) ? "true" : "false");
      break;
    case LUA_TLIGHTUSERDATA:
      PrintLUD(pvalue(o));
      break;
    case LUA_TNUMBER:
      printf(LUA_NUMBER_FMT,nvalue(o));
      break;
    case LUA_TSTRING:
      PrintString(rawtsvalue(o));
      break;
    case LUA_TUI64:
      PrintUI64(ui64value(o));
      break;
    default:				/* cannot happen */
      printf("? type=%d",ttype(o));
      break;
  }
}

#if HKSC_STRUCTURE_EXTENSION_ON
static void PrintStructName(hksc_State *H, short id)
{
  StructProto *p = luaR_getstructbyid(H, id);
  if (p == NULL)
    printf("(unknown struct)");
  else
    printf("%s", getstr(p->name));
}

static void PrintSlotIndex(hksc_State *H, int position)
{
  int slot;
  if (position == 0) return;
  slot = cast_int(luaR_pos2index(H, cast_byte(position)));
  printf("slot %d", slot+1);
}

#endif /* HKSC_STRUCTURE_EXTENSION_ON */

static void PrintCode(hksc_State *H, const Proto *f)
{
  const Instruction *code=f->code;
  int pc,n=f->sizecode;
#if HKSC_STRUCTURE_EXTENSION_ON
  int tagchain = 0;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  for (pc=0; pc<n; pc++)
  {
    Instruction i=code[pc];
    OpCode o=GET_OPCODE(i);
    int a=GETARG_A(i);
    int b=GETARG_B(i);
    int c=GETARG_C(i);
    int bx=GETARG_Bx(i);
    int sbx=GETARG_sBx(i);
    int line=getline(f,pc);
    printf("\t%d\t",pc+1);
    if (line>0) printf("[%d]\t",line); else printf("[-]\t");
    printf("%-9s\t",luaP_opnames[o]);
    switch (getOpMode(o))
    {
      case iABC:
        printf("%d",a);
        if (getBMode(o)!=OpArgN) printf(" %d",ISK(b) ? (-1-INDEXK(b)) : b);
        if (getCMode(o)!=OpArgN) printf(" %d",ISK(c) ? (-1-INDEXK(c)) : c);
        break;
      case iABx:
        if (getBMode(o)==OpArgK) printf("%d %d",a,-1-bx);
        else printf("%d %d",a,bx);
        break;
      case iAsBx:
        if (o==OP_JMP) printf("%d",sbx); else printf("%d %d",a,sbx);
        break;
    }
    switch (o)
    {
      case OP_LOADK:
        printf("\t; "); PrintConstant(f,bx);
        break;
      case OP_GETUPVAL:
      case OP_SETUPVAL:
        printf("\t; %s", (f->sizeupvalues>0) ? getstr(f->upvalues[b]) : "-");
        break;
      case OP_GETGLOBAL: case OP_GETGLOBAL_MEM:
      case OP_SETGLOBAL:
        printf("\t; %s",svalue(&f->k[bx]));
        break;
      case OP_GETTABLE:
      case OP_GETTABLE_S:
      case OP_GETTABLE_N:
      case OP_SELF:
        if (ISK(c))
      case OP_GETFIELD: case OP_GETFIELD_R1:
          { printf("\t; "); PrintConstant(f,INDEXK(c)); }
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
        if (ISK(b) || ISK(c))
        {
          printf("\t; ");
          if (ISK(b)) PrintConstant(f,INDEXK(b)); else printf("-");
          printf(" ");
          if (ISK(c)) PrintConstant(f,INDEXK(c)); else printf("-");
        }
        break;
      case OP_SETFIELD: case OP_SETFIELD_R1:
        printf("\t; ");
        PrintConstant(f,b);
        printf(" ");
        if (ISK(c)) PrintConstant(f,INDEXK(c)); else printf("-");
        break;
      case OP_JMP:
      case OP_FORLOOP:
      case OP_FORPREP:
        printf("\t; to %d",sbx+pc+2);
        break;
      case OP_CLOSURE:
        printf("\t; %p",VOID(f->p[bx]));
        break;
      case OP_SETLIST:
        if (c==0) printf("\t; %d",(int)code[++pc]);
        else printf("\t; %d",c);
        break;
#if HKSC_STRUCTURE_EXTENSION_ON
      case OP_NEWSTRUCT:
        printf("\t; ");
        PrintStructName(H, cast(short, GETARG_Bx(f->code[pc+1])));
        break;
      case OP_SETSLOTN:
      case OP_SETSLOTI:
        printf("\t; ");
        PrintSlotIndex(H, (o == OP_SETSLOTN) ? c : b);
        break;
      case OP_SETSLOT:
        printf("\t; ");
        PrintSlotIndex(H, b);
        printf(" : ");
        printf("%s", luaX_typename(GETARG_Bx(f->code[pc+1])));
        break;
      case OP_SETSLOTS:
        printf("\t; ");
        PrintSlotIndex(H, b);
        printf(" : ");
        PrintStructName(H, cast(short, GETARG_Bx(f->code[pc+1])));
        break;
      case OP_SETSLOTMT:
        tagchain = GET_SLOTMT_TAGCHAIN(i)+1;
        printf("\t; chain %d : ", tagchain);
        if (GET_SLOTMT_TYPE(i) == LUA_TSTRUCT)
          PrintStructName(H, cast(short, GETARG_Bx(f->code[pc+1])));
        else
          printf("%s", luaX_typename(GET_SLOTMT_TYPE(i)));
        break;
      case OP_GETSLOT:
      case OP_GETSLOT_D:
      case OP_SELFSLOT:
        printf("\t; ");
        PrintSlotIndex(H, c);
        break;
      case OP_GETSLOTMT:
      case OP_SELFSLOTMT:
        tagchain = c+1;
        printf("\t; chain %d", tagchain);
        break;
      case OP_DATA:
        if (tagchain) {
          printf("\t; ");
          PrintSlotIndex(H, a);
          if (tagchain > 1) {
            printf(" --> tm ");
            PrintSlotIndex(H, bx);
          }
          tagchain--;
        }
        break;
      case OP_CHECKTYPE:
        printf("\t; %s", luaX_typename(bx));
        break;
      case OP_CHECKTYPES:
      case OP_CHECKTYPE_D:
        printf("\t; ");
        PrintStructName(H, cast(short, bx));
        break;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
#ifdef LUA_CODIW6
      case OP_DELETE: case OP_DELETE_BK:
        if (c == DELETE_UPVAL) {
          printf("\t; %s", (f->sizeupvalues>0) ? getstr(f->upvalues[b]) : "-");
        }
        else if (c == DELETE_GLOBAL) {
          printf("\t; %s",svalue(&f->k[INDEXK(b)]));
        }
        else if (c == DELETE_INDEXED) {
          if (ISK(b))
            printf("\t; "); PrintConstant(f,INDEXK(b));
        }
        break;
#endif /* LUA_CODIW6 */
      default:
        break;
    }
    printf("\n");
  }
  UNUSED(H);
}

#define SS(x)	(x==1)?"":"s"
#define S(x)	x,SS(x)

static void PrintHeader(const Proto *f)
{
  const char *s=getstr(f->source);
  const char *n=f->name ? getstr(f->name) : "(anonymous)";
  if (*s=='@' || *s=='=')
    s++;
  else if (*s==LUA_SIGNATURE[0])
    s="(bstring)";
  else
    s="(string)";
  printf("\n%s <%s:%s:"
#ifdef LUA_CODT6 /* print hash */
         "%" LUA_INT_FRMLEN "x:"
#endif /* LUA_CODT6 */
         "%d,%d> (%d instruction%s, %d bytes at %p)\n",
  	(f->linedefined==0)?"main":"function",s,n,
#ifdef LUA_CODT6
    f->hash,
#endif /* LUA_CODT6 */
    f->linedefined,f->lastlinedefined,
    S(f->sizecode),f->sizecode*Sizeof(Instruction),VOID(f));
  printf("%d%s param%s, %d slot%s, %d upvalue%s, ",
    f->numparams,f->is_vararg?"+":"",SS(f->numparams),
  S(f->maxstacksize),S(f->nups));
    printf("%d local%s, %d constant%s, %d function%s\n",
  S(f->sizelocvars),S(f->sizek),S(f->sizep));
}

static void PrintConstants(const Proto *f)
{
  int i,n=f->sizek;
  printf("constants (%d) for %p:\n",n,VOID(f));
  for (i=0; i<n; i++)
  {
    printf("\t%d\t",i+1);
    PrintConstant(f,i);
    printf("\n");
  }
}

static void PrintLocals(const Proto *f)
{
  int i,n=f->sizelocvars;
  printf("locals (%d) for %p:\n",n,VOID(f));
  for (i=0; i<n; i++)
  {
    printf("\t%d\t%s\t%d\t%d\n", i,
      getstr(f->locvars[i].varname),f->locvars[i].startpc+1,
             f->locvars[i].endpc+1);
  }
}

static void PrintUpvalues(const Proto *f)
{
  int i,n=f->sizeupvalues;
  printf("upvalues (%d) for %p:\n",n,VOID(f));
  if (f->upvalues==NULL) return;
  for (i=0; i<n; i++)
  {
    printf("\t%d\t%s\n",i,getstr(f->upvalues[i]));
  }
}

void PrintFunction(hksc_State *H, const Proto *f, int full)
{
  int i,n=f->sizep;
  PrintHeader(f);
  PrintCode(H, f);
  if (full)
  {
    PrintConstants(f);
    PrintLocals(f);
    PrintUpvalues(f);
  }
  for (i=0; i<n; i++) PrintFunction(H, f->p[i],full);
}
