/*
** $Id: lopcodes.c,v 1.36 2005/10/13 12:22:05 roberto Exp roberto $
** See Copyright Notice in lua.h
*/


#define lopcodes_c
#define LUA_CORE

#include "hksc_begin_code.h"


#include "./lopcodes.h"

/*const char *const hksP_opnames[NUM_OPCODES+1] = {
  "HKS_OPCODE_GETFIELD",
  "HKS_OPCODE_TEST",
  "HKS_OPCODE_CALL_I",
  "HKS_OPCODE_CALL_C",
  "HKS_OPCODE_EQ",
  "HKS_OPCODE_EQ_BK",
  "HKS_OPCODE_GETGLOBAL",
  "HKS_OPCODE_MOVE",
  "HKS_OPCODE_SELF",
  "HKS_OPCODE_RETURN",
  "HKS_OPCODE_GETTABLE_S",
  "HKS_OPCODE_GETTABLE_N",
  "HKS_OPCODE_GETTABLE",
  "HKS_OPCODE_LOADBOOL",
  "HKS_OPCODE_TFORLOOP",
  "HKS_OPCODE_SETFIELD",
  "HKS_OPCODE_SETTABLE_S",
  "HKS_OPCODE_SETTABLE_S_BK",
  "HKS_OPCODE_SETTABLE_N",
  "HKS_OPCODE_SETTABLE_N_BK",
  "HKS_OPCODE_SETTABLE",
  "HKS_OPCODE_SETTABLE_BK",
  "HKS_OPCODE_TAILCALL_I",
  "HKS_OPCODE_TAILCALL_C",
  "HKS_OPCODE_TAILCALL_M",
  "HKS_OPCODE_LOADK",
  "HKS_OPCODE_LOADNIL",
  "HKS_OPCODE_SETGLOBAL",
  "HKS_OPCODE_JMP",
  "HKS_OPCODE_CALL_M",
  "HKS_OPCODE_CALL",
  "HKS_OPCODE_INTRINSIC_INDEX",
  "HKS_OPCODE_INTRINSIC_NEWINDEX",
  "HKS_OPCODE_INTRINSIC_SELF",
  "HKS_OPCODE_INTRINSIC_INDEX_LITERAL",
  "HKS_OPCODE_INTRINSIC_NEWINDEX_LITERAL",
  "HKS_OPCODE_INTRINSIC_SELF_LITERAL",
  "HKS_OPCODE_TAILCALL",
  "HKS_OPCODE_GETUPVAL",
  "HKS_OPCODE_SETUPVAL",
  "HKS_OPCODE_ADD",
  "HKS_OPCODE_ADD_BK",
  "HKS_OPCODE_SUB",
  "HKS_OPCODE_SUB_BK",
  "HKS_OPCODE_MUL",
  "HKS_OPCODE_MUL_BK",
  "HKS_OPCODE_DIV",
  "HKS_OPCODE_DIV_BK",
  "HKS_OPCODE_MOD",
  "HKS_OPCODE_MOD_BK",
  "HKS_OPCODE_POW",
  "HKS_OPCODE_POW_BK",
  "HKS_OPCODE_NEWTABLE",
  "HKS_OPCODE_UNM",
  "HKS_OPCODE_NOT",
  "HKS_OPCODE_LEN",
  "HKS_OPCODE_LT",
  "HKS_OPCODE_LT_BK",
  "HKS_OPCODE_LE",
  "HKS_OPCODE_LE_BK",
  "HKS_OPCODE_LEFT_SHIFT",
  "HKS_OPCODE_LEFT_SHIFT_BK",
  "HKS_OPCODE_RIGHT_SHIFT",
  "HKS_OPCODE_RIGHT_SHIFT_BK",
  "HKS_OPCODE_BIT_AND",
  "HKS_OPCODE_BIT_AND_BK",
  "HKS_OPCODE_BIT_OR",
  "HKS_OPCODE_BIT_OR_BK",
  "HKS_OPCODE_CONCAT",
  "HKS_OPCODE_TESTSET",
  "HKS_OPCODE_FORPREP",
  "HKS_OPCODE_FORLOOP",
  "HKS_OPCODE_SETLIST",
  "HKS_OPCODE_CLOSE",
  "HKS_OPCODE_CLOSURE",
  "HKS_OPCODE_VARARG",
  "HKS_OPCODE_TAILCALL_I_R1",
  "HKS_OPCODE_CALL_I_R1",
  "HKS_OPCODE_SETUPVAL_R1",
  "HKS_OPCODE_TEST_R1",
  "HKS_OPCODE_NOT_R1",
  "HKS_OPCODE_GETFIELD_R1",
  "HKS_OPCODE_SETFIELD_R1",
  "HKS_OPCODE_NEWSTRUCT",
  "HKS_OPCODE_DATA",
  "HKS_OPCODE_SETSLOTN",
  "HKS_OPCODE_SETSLOTI",
  "HKS_OPCODE_SETSLOT",
  "HKS_OPCODE_SETSLOTS",
  "HKS_OPCODE_SETSLOTMT",
  "HKS_OPCODE_CHECKTYPE",
  "HKS_OPCODE_CHECKTYPES",
  "HKS_OPCODE_GETSLOT",
  "HKS_OPCODE_GETSLOTMT",
  "HKS_OPCODE_SELFSLOT",
  "HKS_OPCODE_SELFSLOTMT",
  "HKS_OPCODE_GETFIELD_MM",
  "HKS_OPCODE_CHECKTYPE_D",
  "HKS_OPCODE_GETSLOT_D",
  "HKS_OPCODE_GETGLOBAL_MEM",
  "HKS_OPCODE_MAX",
  NULL
};*/


/* ORDER OP */

const char *const luaP_opnames[NUM_OPCODES+1] = {
  "MOVE",
  "LOADK",
  "LOADBOOL",
  "LOADNIL",
  "GETUPVAL",
  "GETGLOBAL",
  "GETTABLE",
  "SETGLOBAL",
  "SETUPVAL",
  "SETTABLE",
  "NEWTABLE",
  "SELF",
  "ADD",
  "SUB",
  "MUL",
  "DIV",
  "MOD",
  "POW",
  "UNM",
  "NOT",
  "LEN",
  "CONCAT",
  "JMP",
  "EQ",
  "LT",
  "LE",
  "TEST",
  "TESTSET",
  "CALL",
  "TAILCALL",
  "RETURN",
  "FORLOOP",
  "FORPREP",
  "TFORLOOP",
  "SETLIST",
  "CLOSE",
  "CLOSURE",
  "VARARG",
  NULL
};


#define opmode(t,a,b,c,m) (((t)<<7) | ((a)<<6) | ((b)<<4) | ((c)<<2) | (m))

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

