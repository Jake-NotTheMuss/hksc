/*
** $Id: lobject.c $
** Some generic functions over Lua objects
** See Copyright Notice in lua.h
*/

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define lobject_c
#define LUA_CORE

#include "hksclua.h"

#include "ldo.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"



const TValue luaO_nilobject_ = {{NULL}, LUA_TNIL};


/*
** converts an integer to a "floating point byte", represented as
** (eeeeexxx), where the real value is (1xxx) * 2^(eeeee - 1) if
** eeeee != 0 and (xxx) otherwise.
*/
int luaO_int2fb (unsigned int x) {
  int e = 0;  /* expoent */
  while (x >= 16) {
    x = (x+1) >> 1;
    e++;
  }
  if (x < 8) return x;
  else return ((e+1) << 3) | (cast_int(x) - 8);
}


/* converts back */
int luaO_fb2int (int x) {
  int e = (x >> 3) & 31;
  if (e == 0) return x;
  else return ((x & 7)+8) << (e - 1);
}


int luaO_log2 (unsigned int x) {
  static const lu_byte log_2[256] = {
    0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
    6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
  };
  int l = -1;
  while (x >= 256) { l += 8; x >>= 8; }
  return l + log_2[x];

}


int luaO_rawequalObj (const TValue *t1, const TValue *t2) {
  if (ttype(t1) != ttype(t2)) return 0;
  else switch (ttype(t1)) {
    case LUA_TNIL:
      return 1;
    case LUA_TNUMBER:
      return luai_numeq(nvalue(t1), nvalue(t2));
    case LUA_TBOOLEAN:
      return bvalue(t1) == bvalue(t2);  /* boolean true must be 1 !! */
    case LUA_TLIGHTUSERDATA:
      /* the compiler uses half-literals in lightuserdata, not pointers */
      return hlvalue(t1) == hlvalue(t2);
    case LUA_TUI64:
#ifdef LUA_UI64_S
      return ui64value(t1).hi == ui64value(t2).hi &&
              ui64value(t1).lo == ui64value(t2).lo;
#else
      return ui64value(t1) == ui64value(t2);
#endif
    default:
      lua_assert(iscollectable(t1));
      return gcvalue(t1) == gcvalue(t2);
  }
}


int luaO_str2d (const char *s, lua_Number *result) {
  char *endptr;
  *result = lua_str2number(s, &endptr);
  if (endptr == s) return 0;  /* conversion failed */
  if (*endptr == 'x' || *endptr == 'X')  /* maybe an hexadecimal constant? */
    *result = cast_num(strtoul(s, &endptr, 16));
  if (*endptr == '\0') return 1;  /* most common case */
  while (isspace(cast(unsigned char, *endptr))) endptr++;
  if (*endptr != '\0') return 0;  /* invalid trailing characters? */
  return 1;
}


#ifdef LUA_UI64_S
struct lua_ui64_s luaO_str2ui64_s(const char *s, char **endptr, size_t n) {
  struct lua_ui64_s literal;
  const char *lowptr; /* start of low 32 bits */
  char *s1 = cast(char *, s);
  if (n > 8) { /* exceeds 32 bits */
    char *saveptr = s1+n-8;
    char save = *saveptr;
    *saveptr = '\0'; /* just get the high part */
    literal.hi = cast(lu_int32, strtoul(s, endptr, 16));
    *saveptr = save;
    lowptr = cast(const char *, saveptr);
    if (*endptr != saveptr) { /* conversion failed */
      size_t n1; (void)n1;
      badconversion:
      /* store the correct number up to the character that caused the faiure */
      n1 = cast(size_t, *endptr-s1);
      if (n1 > 16)
        literal.hi = literal.lo = cast(lu_int32, 0xfffffffful);
      else {
        literal.lo = literal.hi & cast(lu_int32, 0xfffffffful);
        if (n1 > 8) { /* get the high part again */
          char *saveptr1 = s1+n1-8;
          char save1 = *saveptr1;
          *saveptr1 = '\0';
          literal.hi = cast(lu_int32, strtoul(s, NULL, 16));
          *saveptr1 = save1;
        } else literal.hi = 0;
      }
      return literal;
    }
    if (n > 16) { /* too big to fit in 64 bits */
      /* complete the conversion to know if it succeeds or not */
      strtoul(lowptr, endptr, 16);
      if (*endptr != s1+n) goto badconversion;
      literal.lo = literal.hi = cast(lu_int32, 0xfffffffful); /* overflow */
      return literal;
    }
  } else {
    literal.hi = 0; /* fits in 32 bits */
    lowptr = s;
  }
  literal.lo = cast(lu_int32, strtoul(lowptr, endptr, 16));
  return literal;
}

#define UI64_FORMAT_LOW "%" LUA_INT_FRMLEN "x"
#define UI64_FORMAT_HIGHLOW "%" LUA_INT_FRMLEN "x%08" LUA_INT_FRMLEN "x"

int luaO_ui64_s_2str(char *str, struct lua_ui64_s literal) {
  if (literal.hi == 0)
    return sprintf(str, UI64_FORMAT_LOW, literal.lo);
  else
    return sprintf(str, UI64_FORMAT_HIGHLOW, literal.hi, literal.lo);
}
#endif /* LUA_UI64_S */

int luaO_str2ui64(const char *s, const char *suffix, lu_int64 *result) {
  char *endptr;
  *result = lua_str2ui64(s, &endptr, suffix-s);
  return (endptr == suffix);
}


static void pfn_pushstring (const char *s, size_t n, void *ud) {
  hksc_State *H = ud;
  Mbuffer *b = &G(H)->buff;
  char *p = luaZ_openspace(H, b, luaZ_bufflen(b) + n);
  memcpy(p + luaZ_bufflen(b), s, n);
  luaZ_bufflen(b) += n;
}


/* this function handles only `%d', `%c', %f, %p, and `%s' formats */
const char *luaO_pushvfstring (hksc_State *H, const char *fmt, va_list ap) {
  TString *result;
  Mbuffer *b = &G(H)->buff;
  luaZ_resetbuffer(b);
  luaO_vprintf(pfn_pushstring, H, fmt, ap);
  result = luaS_newlstr(H, luaZ_buffer(b), luaZ_bufflen(b));
  return getstr(result);
}


const char *luaO_pushfstring (hksc_State *H, const char *fmt, ...) {
  const char *msg;
  va_list ap;
  va_start(ap, fmt);
  msg = luaO_pushvfstring(H, fmt, ap);
  va_end(ap);
  return msg;
}


void luaO_chunkid (char *out, const char *source, size_t bufflen) {
  if (source == NULL) {
    if (sizeof("[string \"?\"]") < bufflen)
      bufflen = sizeof("[string \"?\"]");
    strncpy(out, "[string \"?\"]", bufflen);
    out[bufflen-1] = '\0';
  }
  else if (*source == '=') {
    strncpy(out, source+1, bufflen);  /* remove first char */
    out[bufflen-1] = '\0';  /* ensures null termination */
  }
  else {  /* out = "source", or "...source" */
    if (*source == '@') {
      size_t l;
      source++;  /* skip the `@' */
      bufflen -= sizeof(" '...' ");
      l = strlen(source);
      strcpy(out, "");
      if (l > bufflen) {
        source += (l-bufflen);  /* get last part of file name */
        strcat(out, "...");
      }
      strcat(out, source);
    }
    else {  /* out = [string "string"] */
      size_t len = strcspn(source, "\n\r");  /* stop at first newline */
      bufflen -= sizeof(" [string \"...\"] ");
      if (len > bufflen) len = bufflen;
      strcpy(out, "[string \"");
      if (source[len] != '\0') {  /* must truncate? */
        strncat(out, source, len);
        strcat(out, "...");
      }
      else
        strcat(out, source);
      strcat(out, "\"]");
    }
  }
}


const char *luaO_generatechunkname (hksc_State *H, const char *filename) {
  global_State *g = G(H);
  int i;
  for (i = 0; i < g->prefixmaps.used; i++) {
    TString *from = g->prefixmaps.s[i].from;
    TString *to = g->prefixmaps.s[i].to;
    lua_assert(from != NULL && to != NULL);
    if (strncmp(filename, getstr(from), from->tsv.len) == 0) {
      /* remove the `from' part */
      return luaO_pushfstring(H, "@%s%s", getstr(to), filename+from->tsv.len);
    }
  }
  return luaO_pushfstring(H, "@%s", filename);
}
