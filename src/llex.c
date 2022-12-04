/*
** $Id: llex.c,v 2.18 2006/01/23 20:06:19 roberto Exp roberto $
** Lexical Analyzer
** See Copyright Notice in lua.h
*/


#include <locale.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define llex_c
#define LUA_CORE

#include "lua.h"

#include "lctype.h"
#include "ldebug.h"
#include "ldo.h"
#include "llex.h"
#include "lobject.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "lzio.h"


#define next(ls) (ls->current = zgetc(ls->z))




#define currIsNewline(ls)	(ls->current == '\n' || ls->current == '\r')


#define DEFTOK(name, text) text,
const char *const luaX_tokens [] = {
#include "ltoken.def"
  NULL
};
#undef DEFTOK


#define save_and_next(ls) (save(ls, ls->current), next(ls))


static void save (LexState *ls, int c) {
  Mbuffer *b = ls->buff;
  if (b->n + 1 > b->buffsize) {
    size_t newsize;
    if (b->buffsize >= MAX_SIZET/2)
      luaX_lexerror(ls, "lexical element too long", 0);
    newsize = b->buffsize * 2;
    luaZ_resizebuffer(ls->H, b, newsize);
  }
  b->buffer[b->n++] = cast(char, c);
}


void luaX_init (hksc_State *H) {
  int i;
  for (i=0; i<NUM_RESERVED; i++) {
    TString *ts = luaS_new(H, luaX_tokens[i]);
    luaS_fix(ts);  /* reserved words are never collected */
    lua_assert(strlen(luaX_tokens[i])+1 <= TOKEN_LEN);
    ts->tsv.reserved = cast_byte(i+1);  /* reserved word */
  }
}


#define MAXSRC          80

const char *luaX_token2str (LexState *ls, int token) {
  if (token < FIRST_RESERVED) {  /* single-byte symbols? */
    if (lisprint(token))
      return luaO_pushfstring(ls->H, "'%c'", token);
    else  /* control character */
      return luaO_pushfstring(ls->H, "'<\\%d>'", token);
  }
  else {
    const char *s = luaX_tokens[token - FIRST_RESERVED];
    if (token <= LAST_FIXED_FORMAT_TOKEN) /* symbol or reserved word */
      return luaO_pushfstring(ls->H, "'%s'", s);
    else  /* names, strings, and numerals */
      return s;
  }
}


static const char *txtToken (LexState *ls, int token) {
  switch (token) {
    case TK_NAME:
    case TK_STRING:
    case TK_NUMBER:
    case TK_SHORT_LITERAL:
    case TK_LONG_LITERAL:
      save(ls, '\0');
      return luaO_pushfstring(ls->H, "'%s'", luaZ_buffer(ls->buff));
    default:
      return luaX_token2str(ls, token);
  }
}


void luaX_lexerror (LexState *ls, const char *msg, int token) {
  char buff[MAXSRC];
  luaO_chunkid(buff, getstr(ls->source), MAXSRC);
  if (token)
    hksc_setfmsg(ls->H, "%s:%d: %s near %s", buff, ls->linenumber, msg,
                 txtToken(ls, token));
  else
    hksc_setfmsg(ls->H, "%s:%d: %s", buff, ls->linenumber, msg);
  luaD_throw(ls->H, LUA_ERRSYNTAX);
}


void luaX_syntaxerror (LexState *ls, const char *msg) {
  luaX_lexerror(ls, msg, ls->t.token);
}


TString *luaX_newstring (LexState *ls, const char *str, size_t l) {
  hksc_State *H = ls->H;
  TString *ts = luaS_newlstr(H, str, l);
  TValue *o = luaH_setstr(H, ls->fs->h, ts);  /* entry for `str' */
  if (ttisnil(o))
    setbvalue(o, 1);  /* make sure `str' will not be collected */
  return ts;
}


static void inclinenumber (LexState *ls) {
  int old = ls->current;
  lua_assert(currIsNewline(ls));
  next(ls);  /* skip `\n' or `\r' */
  if (currIsNewline(ls) && ls->current != old)
    next(ls);  /* skip `\n\r' or `\r\n' */
  if (++ls->linenumber >= MAX_INT)
    luaX_syntaxerror(ls, "chunk has too many lines");
}


void luaX_setinput (hksc_State *H, LexState *ls, ZIO *z, TString *source) {
  ls->decpoint = '.';
  ls->H = H;
  ls->lookahead.token = TK_EOS;  /* no look-ahead token */
  ls->z = z;
  ls->fs = NULL;
  ls->linenumber = 1;
  ls->lastline = 1;
  ls->source = source;
  luaZ_resizebuffer(ls->H, ls->buff, LUA_MINBUFFER);  /* initialize buffer */
  next(ls);  /* read first char */
}



/*
** =======================================================
** LEXICAL ANALYZER
** =======================================================
*/



static int check_next (LexState *ls, const char *set) {
  if (!strchr(set, ls->current))
    return 0;
  save_and_next(ls);
  return 1;
}


static void buffreplace (LexState *ls, char from, char to) {
  size_t n = luaZ_bufflen(ls->buff);
  char *p = luaZ_buffer(ls->buff);
  while (n--)
    if (p[n] == from) p[n] = to;
}


static void trydecpoint (LexState *ls, SemInfo *seminfo) {
  /* format error: try to update decimal point separator */
  struct lconv *cv = localeconv();
  char old = ls->decpoint;
  ls->decpoint = (cv ? cv->decimal_point[0] : '.');
  buffreplace(ls, old, ls->decpoint);  /* try updated decimal separator */
  if (!luaO_str2d(luaZ_buffer(ls->buff), &seminfo->r)) {
    /* format error with correct decimal point: no more options */
    buffreplace(ls, ls->decpoint, '.');  /* undo change (for error message) */
    luaX_lexerror(ls, "malformed number", TK_NUMBER);
  }
}


static int numeral_type(LexState *ls) {
  char *s = luaZ_buffer(ls->buff);
  size_t n = luaZ_bufflen(ls->buff);
  if (n >= sizeof("0x0hl") && /* minimum size of a valid int literal */
      s[0] == '0' && (s[1] == 'X' || s[1] == 'x') &&
      (s[n-3] == 'H' || s[n-3] == 'h')) {
    switch (s[n-2]) {
      case 'I': case 'i': return TK_SHORT_LITERAL;
      case 'L': case 'l': return TK_LONG_LITERAL;
      default: break;
    }
  }
  return TK_NUMBER; /* default */
}


/* LUA_NUMBER */
static int read_numeral (LexState *ls, SemInfo *seminfo) {
  int token;
  lua_assert(lisdigit(ls->current));
  do {
    save_and_next(ls);
  } while (lisdigit(ls->current) || ls->current == '.');
  if (check_next(ls, "Ee"))  /* `E'? */
    check_next(ls, "+-");  /* optional exponent sign */
  while (lislalnum(ls->current) || ls->current == '_')
    save_and_next(ls);
  save(ls, '\0');
  buffreplace(ls, '.', ls->decpoint);  /* follow locale for decimal point */
  token = numeral_type(ls);
  switch (token) {
    case TK_SHORT_LITERAL:
    case TK_LONG_LITERAL: {
      lu_int64 literal;
      char *s = luaZ_buffer(ls->buff) + 2; /* skip `0x' */
      char *suffix = s + luaZ_bufflen(ls->buff) - 5; /* start of `hl\0' */
      if (!luaO_str2ui64(s, suffix, &literal)) { /* conversion failed */
        ls->buff->n--; /* don't include the null character in messages */
        luaX_lexerror(ls, "malformed int literal", token);
      }
      if (token == TK_LONG_LITERAL && lua_ui64_testlow4bits(literal)) {
        ls->buff->n--; /* don't include the null character in messages */
        luaX_lexerror(ls, "60-bit literal must have lowest 4 bits zero",
                      token);
      }
      if (token == TK_SHORT_LITERAL && sizeof(void *) < sizeof(lu_int64)) {
        const size_t voidpbits = (sizeof(void *) * CHAR_BIT);
        int overflow;
#ifdef LUA_UI64_S
        const size_t int32bits = sizeof(lu_int32)*CHAR_BIT;
        if (int32bits < voidpbits) { /* test high bits */
          const lu_int32 mask =((lu_int32)1 << (voidpbits - int32bits)) - 1;
          overflow=((literal.high & mask) != literal.high);
        } else if (int32bits > voidpbits) {
          const lu_int32 mask = ((lu_int32)1 << voidpbits) - 1;
          overflow=(literal.high != 0) || ((literal.low & mask) != literal.low);
        } else /* int32bits == voidpbits */
          overflow=(literal.high != 0);
#else
        const lu_int64 mask = ((lu_int64)1 << voidpbits) - 1;
        overflow = ((literal & mask) != literal);
#endif
        if (overflow) { /* literal does not fit in a lightuserdata */
          ls->buff->n--; /* don't include the null character in messages */
          luaX_lexerror(ls, "int literal too large for lightuserdata", token);
        }
      }
      seminfo->l = literal;
      break;
    }
    case TK_NUMBER: {
      if (!luaO_str2d(luaZ_buffer(ls->buff), &seminfo->r))  /* format error? */
        trydecpoint(ls, seminfo); /* try to update decimal point separator */
      break;
    }
  }
  return token;
}

static int skip_sep (LexState *ls) {
  int count = 0;
  int s = ls->current;
  lua_assert(s == '[' || s == ']');
  save_and_next(ls);
  while (ls->current == '=') {
    save_and_next(ls);
    count++;
  }
  return (ls->current == s) ? count : (-count) - 1;
}


static void read_long_string (LexState *ls, SemInfo *seminfo, int sep) {
  int cont = 0;
  (void)(cont);  /* avoid warnings when `cont' is not used */
  save_and_next(ls);  /* skip 2nd `[' */
  if (currIsNewline(ls))  /* string starts with a newline? */
    inclinenumber(ls);  /* skip it */
  for (;;) {
    if (!zhasmore(ls->z))
        luaX_lexerror(ls, (seminfo) ? "unfinished long string" :
                                   "unfinished long comment", TK_EOS);
    switch (ls->current) {
#if defined(LUA_COMPAT_LSTR)
      case '[': {
        if (skip_sep(ls) == sep) {
          save_and_next(ls);  /* skip 2nd `[' */
          cont++;
#if LUA_COMPAT_LSTR == 1
          if (sep == 0)
            luaX_lexerror(ls, "nesting of [[...]] is deprecated", '[');
#endif
        }
        break;
      }
#endif
      case ']': {
        if (skip_sep(ls) == sep) {
          save_and_next(ls);  /* skip 2nd `]' */
#if defined(LUA_COMPAT_LSTR) && LUA_COMPAT_LSTR == 2
          cont--;
          if (sep == 0 && cont >= 0) break;
#endif
          goto endloop;
        }
        break;
      }
      case '\n':
      case '\r': {
        save(ls, '\n');
        inclinenumber(ls);
        if (!seminfo) luaZ_resetbuffer(ls->buff);  /* avoid wasting space */
        break;
      }
      default: {
        if (seminfo) save_and_next(ls);
        else next(ls);
      }
    }
  } endloop:
  if (seminfo)
    seminfo->ts = luaX_newstring(ls, luaZ_buffer(ls->buff) + (2 + sep),
                                     luaZ_bufflen(ls->buff) - 2*(2 + sep));
}


static void read_string (LexState *ls, int del, SemInfo *seminfo) {
  save_and_next(ls);
  while (ls->current != del) {
    if (!zhasmore(ls->z))
      luaX_lexerror(ls, "unfinished string", TK_EOS);
    switch (ls->current) {
      case '\n':
      case '\r':
        luaX_lexerror(ls, "unfinished string", TK_STRING);
        continue;  /* to avoid warnings */
      case '\\': {
        int c;
        next(ls);  /* do not save the `\' */
        if (!zhasmore(ls->z))
          continue; /* will raise an error next loop */
        switch (ls->current) {
          case 'a': c = '\a'; break;
          case 'b': c = '\b'; break;
          case 'f': c = '\f'; break;
          case 'n': c = '\n'; break;
          case 'r': c = '\r'; break;
          case 't': c = '\t'; break;
          case 'v': c = '\v'; break;
          case '\n':  /* go through */
          case '\r': save(ls, '\n'); inclinenumber(ls); continue;
          default: {
            if (!lisdigit(ls->current))
              save_and_next(ls);  /* handles \\, \", \', and \? */
            else {  /* \xxx */
              int i = 0;
              c = 0;
              do {
                c = 10*c + (ls->current-'0');
                next(ls);
              } while (++i<3 && lisdigit(ls->current) && zhasmore(ls->z));
              if (c > UCHAR_MAX)
                luaX_lexerror(ls, "escape sequence too large", TK_STRING);
              save(ls, c);
            }
            continue;
          }
        }
        save(ls, c);
        next(ls);
        continue;
      }
      default:
        save_and_next(ls);
    }
  }
  save_and_next(ls);  /* skip delimiter */
  seminfo->ts = luaX_newstring(ls, luaZ_buffer(ls->buff) + 1,
                                   luaZ_bufflen(ls->buff) - 2);
}


static int llex (LexState *ls, SemInfo *seminfo) {
  luaZ_resetbuffer(ls->buff);
  for (;;) {
    if (!zhasmore(ls->z))
      return TK_EOS;
    switch (ls->current) {
      case '\n':
      case '\r': {
        inclinenumber(ls);
        continue;
      }
      case '-': {
        next(ls);
        if (ls->current != '-') return '-';
        /* else is a comment */
        next(ls);
        if (ls->current == '[') {
          int sep = skip_sep(ls);
          luaZ_resetbuffer(ls->buff);  /* `skip_sep' may dirty the buffer */
          if (sep >= 0) {
            read_long_string(ls, NULL, sep);  /* long comment */
            luaZ_resetbuffer(ls->buff);
            continue;
          }
        }
        /* else short comment */
        while (!currIsNewline(ls) && zhasmore(ls->z))
          next(ls);
        continue;
      }
      case '[': {
        int sep = skip_sep(ls);
        if (sep >= 0) {
          read_long_string(ls, seminfo, sep);
          return TK_STRING;
        }
        else if (sep == -1) return '[';
        else luaX_lexerror(ls, "invalid long string delimiter", TK_STRING);
      }
      case '=': {
        next(ls);
        if (ls->current != '=') return '=';
        else { next(ls); return TK_EQ; }
      }
      case '<': {
        next(ls);
        if (ls->current == '=')
          { next(ls); return TK_LE; }
#ifdef LUA_CODT7        /* T7 extension */
        else if (ls->current == '<')
          { next(ls); return TK_LEFT_SHIFT; }
#endif /* LUA_CODT7 */
        else return '<';
      }
      case '>': {
        next(ls);
        if (ls->current == '=')
          { next(ls); return TK_GE; }
#ifdef LUA_CODT7        /* T7 extension */
        else if (ls->current == '>')
          { next(ls); return TK_RIGHT_SHIFT; }
#endif /* LUA_CODT7 */
        else return '>';
      }
      case '~': {
        next(ls);
        if (ls->current != '=') return '~';
        else { next(ls); return TK_NE; }
      }
      case '"':
      case '\'': {
        read_string(ls, ls->current, seminfo);
        return TK_STRING;
      }
      case '.': {
        save_and_next(ls);
        if (check_next(ls, ".")) {
          if (check_next(ls, "."))
            return TK_DOTS;   /* ... */
          else return TK_CONCAT;   /* .. */
        }
        else if (!lisdigit(ls->current)) return '.';
        else {
          int token = read_numeral(ls, seminfo);
          lua_assert(token == TK_NUMBER);
          return token;
        }
      }
      default: {
        if (lisspace(ls->current)) {
          lua_assert(!currIsNewline(ls));
          next(ls);
          continue;
        }
        else if (lisdigit(ls->current)) {
          return read_numeral(ls, seminfo);
        }
        else if (lislalpha(ls->current)) {
          /* identifier or reserved word */
          TString *ts;
          do {
            save_and_next(ls);
          } while (lislalnum(ls->current));
          ts = luaX_newstring(ls, luaZ_buffer(ls->buff),
                                  luaZ_bufflen(ls->buff));
          if (ts->tsv.reserved > 0)  /* reserved word? */ {
            int token = ts->tsv.reserved - 1 + FIRST_RESERVED;
            /* hstructure and hmake are not supported in the cod builds */
            if (token == TK_HSTRUCTURE || token == TK_HMAKE)
            {
              luaG_runerror(ls->H,
                "The reserved words %s and %s can only be used when "
                "the virtual machine is built with structure support.",
                luaX_token2str(ls,TK_HMAKE), luaX_token2str(ls,TK_HSTRUCTURE));
            }
            return token;
          }
          else {
            seminfo->ts = ts;
            return TK_NAME;
          }
        }
        else {
          int c = ls->current;
          next(ls);
          return c;  /* single-char tokens (+ - / ...) */
        }
      }
    }
  }
}


void luaX_next (LexState *ls) {
  ls->lastline = ls->linenumber;
  if (ls->lookahead.token != TK_EOS) {  /* is there a look-ahead token? */
    ls->t = ls->lookahead;  /* use this one */
    ls->lookahead.token = TK_EOS;  /* and discharge it */
  }
  else
    ls->t.token = llex(ls, &ls->t.seminfo);  /* read next token */
}


void luaX_lookahead (LexState *ls) {
  lua_assert(ls->lookahead.token == TK_EOS);
  ls->lookahead.token = llex(ls, &ls->lookahead.seminfo);
}

