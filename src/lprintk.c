/*
** $Id: lprintk.c
** print constants
** See Copyright Notice in lua.h
*/

#include <ctype.h>
#include <stdio.h>

#define lprintk_c
#define LUA_CORE

#include "lprintk.h"
#include "lstate.h"

#define PUTCHAR(c) do {char ch = (c); (*pfn)(&ch, 1, ud);} while (0)

void luaO_printstring (const TString *ts, l_PFN pfn, void *ud, int quote) {
  size_t i;
  PUTCHAR(quote);
  for (i = 0; i < ts->tsv.len; i++) {
    int c = getstr(ts)[i];
    if (c != quote && c != '\\' && isprint(c))
      PUTCHAR(c);
    else {
      PUTCHAR('\\');
      if (c == quote)
        PUTCHAR(quote);
      else switch (c) {
        case '\\': PUTCHAR('\\'); break;
        case '\a': PUTCHAR('a'); break;
        case '\b': PUTCHAR('b'); break;
        case '\f': PUTCHAR('f'); break;
        case '\n': PUTCHAR('n'); break;
        case '\r': PUTCHAR('r'); break;
        case '\t': PUTCHAR('t'); break;
        case '\v': PUTCHAR('v'); break;
        default: {
          char buff [5];
          int n = sprintf(buff, "%03u", cast(unsigned char, c));
          (*pfn)(buff, cast(size_t, n), ud);
        }
      }
    }
  } 
  PUTCHAR(quote);
}

/* print a constant value as it would appear in Lua code */
void luaO_printk (const TValue *o,
                  void (*pfn) (const char *s, size_t l, void *ud),
                  void *ud, int quote) {
  switch (ttype(o)) {
    char buff[LUAI_MAXNUMBER2STR+LUAI_MAXUI642STR+1];
    int n;
    char lsuf;
    lu_int64 ui64;
    case LUA_TNIL: (*pfn)("nil", 3, ud); break;
    case LUA_TBOOLEAN:
      if (bvalue(o)) (*pfn)("true", 4, ud);
      else (*pfn)("false", 5, ud);
      break;
#ifdef HKSC_VERSION
    case LUA_TLIGHTUSERDATA: {
      size_t hl = hlvalue(o);
      lsuf = 'i';
#ifndef LUA_UI64_S
      ui64 = cast(lu_int64, hl);
#else
      ui64.lo = hl & 0xffffffff;
      if (sizeof(size_t) * CHAR_BIT > 32)
        ui64.hi = hl >> 32;
      else
        ui64.hi = 0;
#endif /* LUA_UI64_S */
      goto printui64;
    }
#endif /* HKSC_VERSION */
    case LUA_TNUMBER:
      n = lua_number2str(buff, nvalue(o));
      (*pfn)(buff, n, ud);
      break;
    case LUA_TSTRING:
      luaO_printstring(rawtsvalue(o), pfn, ud, quote);
      break;
#ifdef HKSC_VERSION
    case LUA_TUI64:
      lsuf = 'l';
      ui64 = ui64value(o);
      printui64: n = lua_ui642str(buff + 2, ui64) + 2;
      buff[0] = '0', buff[1] = 'x';
      buff[n++] = 'h', buff[n++] = lsuf;
      buff[n] = 0;
      (*pfn)(buff, n, ud);
      break;
#endif /* HKSC_VERSION */
    default: lua_assert(0);
  }
}
