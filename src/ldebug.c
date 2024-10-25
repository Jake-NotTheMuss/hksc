/*
** $Id: ldebug.c $
** Debug Interface
** See Copyright Notice in lua.h
*/


#include <stdarg.h>
#include <stddef.h>
#include <string.h>


#define ldebug_c
#define LUA_CORE

#include "hksclua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"


/*
** {======================================================
** Symbolic Execution and code checker
** =======================================================
*/

#define check(x)    if (!(x)) return 0;

#define checkjump(pt,pc)  check(0 <= pc && pc < pt->sizecode)

#define checkreg(pt,reg)  check((reg) < (pt)->maxstacksize)

#define checktype(type)  check((type) >= LUA_TNIL && (type) <= LUA_TSTRUCT)

/* check TYPE is a builtin type (excludes LUA_TSTRUCT) */
#define checktypeb(type)  check((type) >= LUA_TNIL && (type) < LUA_TSTRUCT)

#define checkslotposition(pos)  check((pos) >= 0 && (pos) < 256)



static int precheck (const Proto *pt) {
  check(pt->maxstacksize <= MAXSTACK);
  lua_assert(pt->numparams+(pt->is_vararg & VARARG_HASARG) <= pt->maxstacksize);
  lua_assert(!(pt->is_vararg & VARARG_NEEDSARG) ||
              (pt->is_vararg & VARARG_HASARG));
  check(pt->sizeupvalues <= pt->nups);
  check(pt->sizelineinfo == pt->sizecode || pt->sizelineinfo == 0);
  check(GET_OPCODE(pt->code[pt->sizecode-1]) == OP_RETURN);
  return 1;
}


#define checkopenop(pt,pc)  luaG_checkopenop((pt)->code[(pc)+1])

int luaG_checkopenop (Instruction i) {
  switch (GET_OPCODE(i)) {
    case OP_CALL: case OP_CALL_I: case OP_CALL_I_R1: case OP_CALL_C:
    case OP_CALL_M: case OP_TAILCALL: case OP_TAILCALL_I: case OP_TAILCALL_I_R1:
    case OP_TAILCALL_C: case OP_TAILCALL_M:
    case OP_RETURN:
    case OP_SETLIST: {
      check(GETARG_B(i) == 0);
      return 1;
    }
    default: return 0;  /* invalid instruction after an open call */
  }
}


static int checkArgMode (const Proto *pt, int r, enum OpArgMask mode) {
  switch (mode) {
    case OpArgN: check(r == 0); break;
    case OpArgU: case OpArgK: case OpArgUK: break;
    case OpArgR: checkreg(pt, r); break;
    case OpArgRK:
      check(ISK(r) ? INDEXK(r) < pt->sizek : r < pt->maxstacksize);
      break;
    default: break;
  }
  return 1;
}


static Instruction symbexec (const Proto *pt, int lastpc, int reg) {
  int pc;
  int last;  /* stores position of last instruction that changed `reg' */
  last = pt->sizecode-1;  /* points to final return (a `neutral' instruction) */
  check(precheck(pt));
  for (pc = 0; pc < lastpc; pc++) {
    Instruction i = pt->code[pc];
    OpCode op = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = 0;
    int c = 0;
    check(op < NUM_OPCODES);
    if (op != OP_DATA) checkreg(pt, a);
    switch (getOpMode(op)) {
      case iABC: {
        b = GETARG_B(i);
        c = GETARG_C(i);
        check(checkArgMode(pt, b, getBMode(op)));
        check(checkArgMode(pt, c, getCMode(op)));
        break;
      }
      case iABx: {
        b = GETARG_Bx(i);
        if (getBMode(op) == OpArgK) check(b < pt->sizek);
        break;
      }
      case iAsBx: {
        b = GETARG_sBx(i);
        if (getBMode(op) == OpArgUK) {
          int dest = pc+1+b;
          check(0 <= dest && dest < pt->sizecode);
          if (dest > 0) {
            /* cannot jump to a setlist count */
            Instruction d = pt->code[dest-1];
            check(!(GET_OPCODE(d) == OP_SETLIST && GETARG_C(d) == 0));
          }
        }
        break;
      }
    }
    if (testAMode(op)) {
      if (a == reg) last = pc;  /* change register `a' */
    }
    if (testTMode(op)) {
      check(pc+2 < pt->sizecode);  /* check skip */
      check(GET_OPCODE(pt->code[pc+1]) == OP_JMP);
    }
    switch (op) {
      case OP_LOADBOOL: {
        check(c == 0 || pc+2 < pt->sizecode);  /* check its jump */
        if (c == 1) {  /* does it jump? */
          check(pc+2 < pt->sizecode);  /* check its jump */
          check(GET_OPCODE(pt->code[pc+1]) != OP_SETLIST ||
                GETARG_C(pt->code[pc+1]) != 0);
        }
        break;
      }
      case OP_LOADNIL: {
        if (a <= reg && reg <= b)
          last = pc;  /* set registers from `a' to `b' */
        break;
      }
      case OP_GETUPVAL:
      case OP_SETUPVAL:
      case OP_SETUPVAL_R1: {
        check(b < pt->nups);
        break;
      }
      case OP_GETFIELD: case OP_GETFIELD_R1: {
        check(ttisstring(&pt->k[c]));
        break;
      }
      case OP_GETGLOBAL:
      case OP_GETGLOBAL_MEM:
      case OP_SETGLOBAL:
      case OP_SETFIELD:
      case OP_SETFIELD_R1: {
        check(ttisstring(&pt->k[b]));
        break;
      }
      case OP_SELF: {
        checkreg(pt, a+1);
        if (reg == a+1) last = pc;
        break;
      }
      case OP_CONCAT: {
        check(b < c);  /* at least two operands */
        break;
      }
      case OP_TFORLOOP: {
        check(c >= 1);  /* at least one result (control variable) */
        checkreg(pt, a+2+c);  /* space for results */
        if (reg >= a+2) last = pc;  /* affect all regs above its base */
        break;
      }
      case OP_FORLOOP:
      case OP_FORPREP:
        checkreg(pt, a+3);
        /* fallthrough */
      case OP_JMP: {
        int dest = pc+1+b;
        /* not full check and jump is forward and do not skip `lastpc'? */
        if (reg != NO_REG && pc < dest && dest <= lastpc)
          pc += b;  /* do the jump */
        break;
      }
      case CASE_OP_CALL_LABEL: {
        if (b != 0) {
          checkreg(pt, a+b-1);
        }
        c--;  /* c = num. returns */
        if (c == LUA_MULTRET) {
          check(checkopenop(pt, pc));
        }
        else if (c != 0)
          checkreg(pt, a+c-1);
        if (reg >= a) last = pc;  /* affect all registers above base */
        break;
      }
      case OP_RETURN: {
        b--;  /* b = num. returns */
        if (b > 0) checkreg(pt, a+b-1);
        break;
      }
      case OP_SETLIST: {
        if (b > 0) checkreg(pt, a + b);
        if (c == 0) {
          pc++;
          check(pc < pt->sizecode - 1);
        }
        break;
      }
      case OP_CLOSURE: {
        int nup, j;
        check(b < pt->sizep);
        nup = pt->p[b]->nups;
        check(pc + nup < pt->sizecode);
        for (j = 1; j <= nup; j++) {
          OpCode op1 = GET_OPCODE(pt->code[pc + j]);
          int a1 = GETARG_A(pt->code[pc + j]);
          check(op1 == OP_DATA);
          check(a1 == 1 || a1 == 2);
        }
        if (reg != NO_REG)  /* tracing? */
          pc += nup;  /* do not 'execute' these pseudo-instructions */
        break;
      }
      case OP_VARARG: {
        check((pt->is_vararg & VARARG_ISVARARG) &&
             !(pt->is_vararg & VARARG_NEEDSARG));
        b--;
        if (b == LUA_MULTRET) check(checkopenop(pt, pc));
        checkreg(pt, a+b-1);
        break;
      }
#if HKSC_STRUCTURE_EXTENSION_ON
      /* NOTE: certain codes are checked when serializing structures;
         - OP_NEWSTRUCT is checked
         - OP_CHECKTYPES and OP_CHECKTYPE_D are checked
         - OP_SETSLOTS only needs args checked
         - OP_SETSLOTMT only needs args and tag-chain codes checked */
      case OP_CHECKTYPE: {
        checktypeb(b);
        break;
      }
      case OP_SETSLOT: {
        check(pc+1 < pt->sizecode);  /* need data code */
        check(GET_OPCODE(pt->code[pc+1]) == OP_DATA)
        checktypeb(GETARG_Bx(pt->code[pc+1]));
      }
      /* fallthrough */
      case OP_SETSLOTI:
      case OP_SETSLOTS: {
        checkslotposition(b);
        break;
      }
      case OP_SETSLOTN: {
        checkslotposition(c);
        break;
      }
      case OP_GETSLOT:
      case OP_GETSLOT_D: {
        checkslotposition(c);
        break;
      }
      case OP_SELFSLOT: {
        checkslotposition(c);
        checkreg(pt, a+1);
        if (reg == a+1) last = pc;
        break;
      }
      case OP_SETSLOTMT: {
        checktype(GET_SLOTMT_TYPE(i));
      }
      /* fallthrough */
      case OP_GETSLOTMT:
      case OP_SELFSLOTMT: {
        int ntag = (op == OP_SETSLOTMT) ? GET_SLOTMT_TAGCHAIN(i) : c;
        int extra = op == OP_SETSLOTMT && GET_SLOTMT_TYPE(i) == LUA_TSTRUCT;
        int n;
        check(pc+extra+1+ntag < pt->sizecode);
        for (n = 0; n <= ntag; n++) {
          Instruction data = pt->code[pc+extra+n+1];
          check(GET_OPCODE(data) == OP_DATA);
          checkslotposition(GETARG_A(data));
          checkslotposition(GETARG_Bx(data));
        }
        if (op == OP_SELFSLOTMT) {
          checkreg(pt, a+1);
          if (reg == a+1) last = pc;
        }
        break;
      }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
      default: break;
    }
  }
  return pt->code[last];
}

#undef check
#undef checkjump
#undef checkreg

/* }====================================================== */


int luaG_checkcode (const Proto *pt) {
  return (symbexec(pt, pt->sizecode, NO_REG) != 0);
}


void luaG_runerror (hksc_State *H, const char *fmt, ...) {
  va_list argp;
  va_start(argp, fmt);
  luaD_setvferror(H, fmt, argp);
  va_end(argp);
  luaD_throw(H, LUA_ERRRUN);
}

