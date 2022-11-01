/*
** $Id: lopcodes.c,v 1.36 2005/10/13 12:22:05 roberto Exp roberto $
** See Copyright Notice in lua.h
*/


#define lopcodes_c
#define LUA_CORE

#include "hksc_begin_code.h"


#include "./lopcodes.h"


/* ORDER OP */

const char *const luaP_opnames[NUM_OPCODES+1] = {
  "GETFIELD",
  "TEST",
  "CALL_I",
  "CALL_C",
  "EQ",
  "EQ_BK",
  "GETGLOBAL",
  "MOVE",
  "SELF",
  "RETURN",
  "GETTABLE_S",
  "GETTABLE_N",
  "GETTABLE",
  "LOADBOOL",
  "TFORLOOP",
  "SETFIELD",
  "SETTABLE_S",
  "SETTABLE_S_BK",
  "SETTABLE_N",
  "SETTABLE_N_BK",
  "SETTABLE",
  "SETTABLE_BK",
  "TAILCALL_I",
  "TAILCALL_C",
  "TAILCALL_M",
  "LOADK",
  "LOADNIL",
  "SETGLOBAL",
  "JMP",
  "CALL_M",
  "CALL",
  "INTRINSIC_INDEX",
  "INTRINSIC_NEWINDEX",
  "INTRINSIC_SELF",
  "INTRINSIC_INDEX_LITERAL",
  "INTRINSIC_NEWINDEX_LITERAL",
  "INTRINSIC_SELF_LITERAL",
  "TAILCALL",
  "GETUPVAL",
  "SETUPVAL",
  "ADD",
  "ADD_BK",
  "SUB",
  "SUB_BK",
  "MUL",
  "MUL_BK",
  "DIV",
  "DIV_BK",
  "MOD",
  "MOD_BK",
  "POW",
  "POW_BK",
  "NEWTABLE",
  "UNM",
  "NOT",
  "LEN",
  "LT",
  "LT_BK",
  "LE",
  "LE_BK",
  "LEFT_SHIFT",
  "LEFT_SHIFT_BK",
  "RIGHT_SHIFT",
  "RIGHT_SHIFT_BK",
  "BIT_AND",
  "BIT_AND_BK",
  "BIT_OR",
  "BIT_OR_BK",
  "CONCAT",
  "TESTSET",
  "FORPREP",
  "FORLOOP",
  "SETLIST",
  "CLOSE",
  "CLOSURE",
  "VARARG",
  "TAILCALL_I_R1",
  "CALL_I_R1",
  "SETUPVAL_R1",
  "TEST_R1",
  "NOT_R1",
  "GETFIELD_R1",
  "SETFIELD_R1",
  "NEWSTRUCT",
  "DATA",
  "SETSLOTN",
  "SETSLOTI",
  "SETSLOT",
  "SETSLOTS",
  "SETSLOTMT",
  "CHECKTYPE",
  "CHECKTYPES",
  "GETSLOT",
  "GETSLOTMT",
  "SELFSLOT",
  "SELFSLOTMT",
  "GETFIELD_MM",
  "CHECKTYPE_D",
  "GETSLOT_D",
  "GETGLOBAL_MEM",
  NULL
};


#define opmode(t,a,b,c,m) (((t)<<7) | ((a)<<6) | ((b)<<4) | ((c)<<2) | (m))

/* T7 TODO */
const lu_byte luaP_opmodes[NUM_OPCODES] = {
/*       T  A    B       C     mode		   opcode	*/
  opmode(0, 1, OpArgR, OpArgN, iABC) 		/* OP_MOVE */
 ,opmode(0, 1, OpArgK, OpArgN, iABx)		/* OP_LOADK */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)		/* OP_LOADBOOL */
 ,opmode(0, 1, OpArgR, OpArgN, iABC)		/* OP_LOADNIL */
 ,opmode(0, 1, OpArgU, OpArgN, iABC)		/* OP_GETUPVAL */
 ,opmode(0, 1, OpArgK, OpArgN, iABx)		/* OP_GETGLOBAL */
 ,opmode(0, 1, OpArgR, OpArgK, iABC)		/* OP_GETTABLE */
 ,opmode(0, 0, OpArgK, OpArgN, iABx)		/* OP_SETGLOBAL */
 ,opmode(0, 0, OpArgU, OpArgN, iABC)		/* OP_SETUPVAL */
 ,opmode(0, 0, OpArgK, OpArgK, iABC)		/* OP_SETTABLE */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)		/* OP_NEWTABLE */
 ,opmode(0, 1, OpArgR, OpArgK, iABC)		/* OP_SELF */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_ADD */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_SUB */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_MUL */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_DIV */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_MOD */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_POW */
 ,opmode(0, 1, OpArgR, OpArgN, iABC)		/* OP_UNM */
 ,opmode(0, 1, OpArgR, OpArgN, iABC)		/* OP_NOT */
 ,opmode(0, 1, OpArgR, OpArgN, iABC)		/* OP_LEN */
 ,opmode(0, 1, OpArgR, OpArgR, iABC)		/* OP_CONCAT */
 ,opmode(0, 0, OpArgR, OpArgN, iAsBx)		/* OP_JMP */
 ,opmode(1, 0, OpArgK, OpArgK, iABC)		/* OP_EQ */
 ,opmode(1, 0, OpArgK, OpArgK, iABC)		/* OP_LT */
 ,opmode(1, 0, OpArgK, OpArgK, iABC)		/* OP_LE */
 ,opmode(1, 1, OpArgR, OpArgU, iABC)		/* OP_TEST */
 ,opmode(1, 1, OpArgR, OpArgU, iABC)		/* OP_TESTSET */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)		/* OP_CALL */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)		/* OP_TAILCALL */
 ,opmode(0, 0, OpArgU, OpArgN, iABC)		/* OP_RETURN */
 ,opmode(0, 1, OpArgR, OpArgN, iAsBx)		/* OP_FORLOOP */
 ,opmode(0, 1, OpArgR, OpArgN, iAsBx)		/* OP_FORPREP */
 ,opmode(1, 0, OpArgN, OpArgU, iABC)		/* OP_TFORLOOP */
 ,opmode(0, 0, OpArgU, OpArgU, iABC)		/* OP_SETLIST */
 ,opmode(0, 0, OpArgN, OpArgN, iABC)		/* OP_CLOSE */
 ,opmode(0, 1, OpArgU, OpArgN, iABx)		/* OP_CLOSURE */
 ,opmode(0, 1, OpArgU, OpArgN, iABC)		/* OP_VARARG */
};

