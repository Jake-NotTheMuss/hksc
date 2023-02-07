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
      return pvalue(t1) == pvalue(t2);
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


/*
** `stringbuilder' holds data needed by `luaO_pushvfstring'
*/
struct StringBuilder {
  hksc_State *H;
  char *str;  /* currently building string */
  size_t l;  /* current length of string */
  TString *result;  /* final result */
  const char *data;
  union {
    va_list argp; /* extra arguments in a formatted string */
    size_t len; /* length of data */
  } aux;
};


#define pushstr(sb,str) pushlstr(sb,str,strlen(str))
#define pushchar(sb,c) do { char buff = (c); pushlstr(sb,&buff,1); } while (0)

static void pushlstr (struct StringBuilder *sb, const char *str, size_t l) {
  hksc_State *H = sb->H;
  size_t ol = sb->l;
  size_t nl = ol + l;
  luaM_reallocvector(H, sb->str, ol, nl, char);
  memcpy(&sb->str[ol], str, l);
  sb->l = nl;
}

#define pushnum(sb,n) do { \
  char s[LUAI_MAXNUMBER2STR]; \
  lua_number2str(s, cast_num(n)); \
  pushstr(sb, s); \
} while (0)

#define pushint(sb,n,fmt) do { \
  char s[(CHAR_BIT * sizeof(int) - 1) / 3 + 2]; \
  sprintf(s, fmt, n); \
  pushstr(sb, s); \
} while (0)


/* return a printable version of a string constant */
static void f_kstring2print (hksc_State *H, void *ud) {
  struct StringBuilder *sb = (struct StringBuilder *)ud;
  int i,n=sb->aux.len;
  const char *s = sb->data; /* original string */
  pushchar(sb, '\"');
  for (i = 0; i < n; i++) {
    int c = s[i];
    switch (c) {
      case '"': pushchar(sb, '\\'); pushchar(sb, '"'); break;
      case '\\': pushchar(sb, '\\'); pushchar(sb, '\\'); break;
      case '\a': pushchar(sb, '\\'); pushchar(sb, 'a'); break;
      case '\b': pushchar(sb, '\\'); pushchar(sb, 'b'); break;
      case '\f': pushchar(sb, '\\'); pushchar(sb, 'f'); break;
      case '\n': pushchar(sb, '\\'); pushchar(sb, 'n'); break;
      case '\r': pushchar(sb, '\\'); pushchar(sb, 'r'); break;
      case '\t': pushchar(sb, '\\'); pushchar(sb, 't'); break;
      case '\v': pushchar(sb, '\\'); pushchar(sb, 'v'); break;
      default: {
        if (isprint(c))
          pushchar(sb, c);
        else {
          char buff[5];
          sprintf(buff, "\\%03u", (unsigned char)c);
          pushstr(sb, buff);
        }
      }
    }
  }
  pushchar(sb, '\"');
  sb->result = luaS_newlstr(H, sb->str, sb->l);
}

TString *luaO_kstring2print (hksc_State *H, TString *ts) {
  int status;
  struct StringBuilder sb;
  sb.H=H; sb.str=NULL; sb.l=0; sb.result=NULL; sb.data=getstr(ts);
  sb.aux.len=ts->tsv.len;
  status = luaD_pcall(H, f_kstring2print, &sb);
  luaM_freearray(H, sb.str, sb.l, char);
  if (status) /* memory error */
    luaD_throw(H, status); /* pass error to caller */
  else /* good */
    lua_assert(sb.result != NULL);
  return sb.result;
}



/* this function handles only `%d', `%c', %f, %p, and `%s' formats */
static void f_buildvfstring (hksc_State *H, void *ud) {
  struct StringBuilder *sb = (struct StringBuilder *)ud;
  const char *fmt = sb->data;
  va_list argp = sb->aux.argp;
  for (;;) {
    const char *e = strchr(fmt, '%');
    if (e == NULL) break;
    pushlstr(sb, fmt, e-fmt);
    switch (*(e+1)) {
      case 's': {
        const char *s = va_arg(argp, char *);
        if (s == NULL) s = "(null)";
        pushstr(sb, s);
        break;
      }
      case 'c': {
        pushchar(sb, cast(char, va_arg(argp, int)));
        break;
      }
      case 'd': {
        pushint(sb, va_arg(argp, int), "%d");
        break;
      }
      case 'u': {
        pushint(sb, va_arg(argp, unsigned int), "%u");
        break;
      }
      case 'f': {
        pushnum(sb, va_arg(argp, l_uacNumber));
        break;
      }
      case 'p': {
        char buff[4*sizeof(void *) + 8]; /* should be enough space for a `%p' */
        sprintf(buff, "%p", va_arg(argp, void *));
        pushstr(sb, buff);
        break;
      }
      case '%': {
        pushchar(sb, '%');
        break;
      }
      default: {
        char buff[2];
        buff[0] = '%';
        buff[1] = *(e+1);
        pushlstr(sb, buff, sizeof(buff));
        break;
      }
    }
    fmt = e+2;
  }
  pushstr(sb, fmt);
  sb->result = luaS_newlstr(H, sb->str, sb->l);
}


const char *luaO_pushvfstring (hksc_State *H, const char *fmt, va_list argp) {
  int status;
  struct StringBuilder sb;
  sb.H=H; sb.str=NULL; sb.l=0; sb.result=NULL; sb.data=fmt; sb.aux.argp=argp;
  status = luaD_pcall(H, f_buildvfstring, &sb);
  luaM_freearray(H, sb.str, sb.l, char);
  if (status) /* memory error */
    luaD_throw(H, status); /* pass error to caller */
  else /* good */
    lua_assert(sb.result != NULL);
  return getstr(sb.result);
}


const char *luaO_pushfstring (hksc_State *H, const char *fmt, ...) {
  const char *msg;
  va_list argp;
  va_start(argp, fmt);
  msg = luaO_pushvfstring(H, fmt, argp);
  va_end(argp);
  return msg;
}


void luaO_chunkid (char *out, const char *source, size_t bufflen) {
  if (*source == '=') {
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
