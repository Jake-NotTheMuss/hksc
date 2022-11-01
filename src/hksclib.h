/*
** hksclib.h
** A wrapper for the modded Lua library
** See Copyright Notice in lua.h
*/


#ifndef HKSC_LIB_H
#define HKSC_LIB_H

#include "luaconf.h"
#include "lua.h"

#include "lstate.h"

#if 0
/*
** int literal options
**
** NONE : Disable int literals, lexer will emit an error when encountered
** LUD  : Enable only 32-bit int literals, stored as type TLIGHTUSERDATA
** UI64 : Enable only 60-bit int literals, stored as type TUI64
** ALL  : Enable both LUD and UI64 int literals
*/
#define INT_LITERALS_NONE   0
#define INT_LITERALS_LUD    1
#define INT_LITERALS_UI64   2
#define INT_LITERALS_ALL    3

typedef int (*lua_LineMap)(const char *, int);


/*
** bytecode sharing modes
**
** OFF  :
** ON   :
** SECURE :
*/
#define HKSC_SHARING_OFF  0
#define HKSC_SHARING_ON   1
#define HKSC_SHARING_SECURE 2


/*
** bytecode sharing formats
**
** DEFAULT  :
** INPLACE  :
** REFERENCED :
*/
#define HKSC_BYTECODE_DEFAULT  0
#define HKSC_BYTECODE_INPLACE  1
#define HKSC_BYTECODE_REFERENCED 2

/*
** bytecode endianness
**
** DEFAULT  : Dump/undump code as system endianness
** BIG      : Dump/undump code as big endian
** LITTLE   : Dump/undump code as little endian
*/
#define HKSC_DEFAULT_ENDIAN   0
#define HKSC_BIG_ENDIAN       1
#define HKSC_LITTLE_ENDIAN    2

/*
** hksc_Settings - global settings shared by all states
*/
typedef struct hksc_Settings
{
  /* general settings */
  int endian; /* bytecode endianness */
  int sharing_format; /* bytecode sharing format, default = INPLACE */
  int sharing_mode; /* bytecode sharing mode, default = ON */

  /* compiler-specific settings */
  int emit_struct; /* whether `hstructure' and `hmake' should be emitted */
  int literals; /* int literal setting */
  const char **strip_names;
  lua_LineMap debug_map;

  /* decompiler-specific settings */
  int ignore_debug; /* ignore debug info if present */
  int match_line_info; /* emit statements according to the line mapping */
} hksc_Settings;


/* hksc modes - either compiling source or decompiling bytecode */
#define HKSC_MODE_COMPILE   0
#define HKSC_MODE_DECOMPILE 1

/*
** hksc_State - all data which characterizes an hksc state
*/
typedef struct hksc_State
{
  int mode; /* compiling or decompiling? */
  hksc_Settings *settings; /* global compiler settings */
} hksc_State;
#endif
LUAI_FUNC hksc_State *hksc_xnewstate(void);
LUAI_FUNC int hksc_parsefile(hksc_State *H, const char *filename);

/* extra error code for `luaL_load' */
#define LUA_ERRFILE     (LUA_ERRERR+1)

#endif /* HKSC_LIB_H */
