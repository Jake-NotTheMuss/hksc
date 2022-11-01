/*
** $Id: llex.c,v 2.18 2006/01/23 20:06:19 roberto Exp roberto $
** Lexical Analyzer
** See Copyright Notice in lua.h
*/


#include <ctype.h>
#include <locale.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "hksc_begin_code.h"

#define llex_c
#define LUA_CORE

#include "lua.h"

#include "lctype.h"
#include "ldebug.h"
#include "lerror.h"
#include "llex.h"
#include "lobject.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "lzio.h"


#define next(ls) (ls->current = zgetc(ls->z))




#define currIsNewline(ls)	(ls->current == '\n' || ls->current == '\r')


#define DEFTOK(name, text) text,
#define DEFTOK1(name, text) DEFTOK(name, text)
const char *const luaX_tokens [] = {
#include "ltokens.def"
  NULL
};
#undef DEFTOK1
#undef DEFTOK

/* used by luaX_token2str */
#define FIRST_NONQUOTED_TOKEN LTOKENS_FIRST_NONQUOTED

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
    if (token < FIRST_NONQUOTED_TOKEN) /* fixed format
                                            (symbols and reserved words)? */
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
    hksc_setfmsg(ls->H, "%s:%d: %s near %s", buff,
                       ls->linenumber, msg, txtToken(ls, token));
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


static int numeral_type(LexState *ls)
{
  size_t n = ls->buff->n;
  char *buf = ls->buff->buffer;

  if (n >= 6)
  {
    if (buf[0] == '0' && (buf[1] == 'X' || buf[1] == 'x'))
    {
      /* end of string should be "hi\0" or "hl\0" */
      char c = buf[n - 3];
      if (c == 'H' || c == 'h')
      {
        c = buf[n - 2];
        if (c == 'I' || c == 'i')
          return TK_SHORT_LITERAL;
        else if (c == 'L' || c == 'l')
          return TK_LONG_LITERAL;
      }
    }
  }
  /* default */
  return TK_NUMBER;
}

/* LUA_NUMBER */
static int read_numeral (LexState *ls, SemInfo *seminfo) {
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

  int token = numeral_type(ls);

  switch (token)
  {
    case TK_SHORT_LITERAL:
    case TK_LONG_LITERAL:
    {
      char *s = luaZ_buffer(ls->buff) + 2; /* skip "0x" */
      size_t n = luaZ_bufflen(ls->buff);
      char *endptr;
      lua_Literal l = strtoull(s, &endptr, 16);

      /* note that -5 is used because endptr should point to the start of
         the suffix:
         &s[n - 3] points to the null character (-2 for the skipped "0x")
         and an additional -2 points to the 'h' */
      if (endptr == s || endptr != &s[n - 5])
      {
        ls->buff->n--; /* don't include the null character in messages */
        luaX_lexerror(ls, "malformed int literal", token);
      }

      /* for a long literal, need to check lowest 4 bits are 0 */
      if (token == TK_LONG_LITERAL)
      {
        if ((l & 0xf) != 0)
        {
          ls->buff->n--; /* don't include the null character in messages */
          luaX_lexerror(ls, "60-bit literal must have lowest 4 bits zero",
                        token);
        }
      }
      seminfo->l = l;
    }
    case TK_NUMBER:
    {
      if (!luaO_str2d(luaZ_buffer(ls->buff), &seminfo->r))  /* format error? */
        trydecpoint(ls, seminfo); /* try to update decimal point separator */
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
    switch (ls->current) {
      case EOZ:
        luaX_lexerror(ls, (seminfo) ? "unfinished long string" :
                                   "unfinished long comment", TK_EOS);
        break;  /* to avoid warnings */
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
    switch (ls->current) {
      case EOZ:
        luaX_lexerror(ls, "unfinished string", TK_EOS);
        continue;  /* to avoid warnings */
      case '\n':
      case '\r':
        luaX_lexerror(ls, "unfinished string", TK_STRING);
        continue;  /* to avoid warnings */
      case '\\': {
        int c;
        next(ls);  /* do not save the `\' */
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
          case EOZ: continue;  /* will raise an error next loop */
          default: {
            if (!lisdigit(ls->current))
              save_and_next(ls);  /* handles \\, \", \', and \? */
            else {  /* \xxx */
              int i = 0;
              c = 0;
              do {
                c = 10*c + (ls->current-'0');
                next(ls);
              } while (++i<3 && lisdigit(ls->current));
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
        while (!currIsNewline(ls) && ls->current != EOZ)
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
        /* T7 extension */
        else if (ls->current == '<')
          { next(ls); return TK_LEFT_SHIFT; }
        else return '<';
      }
      case '>': {
        next(ls);
        if (ls->current == '=')
          { next(ls); return TK_GE; }
        /* T7 extension */
        else if (ls->current == '>')
          { next(ls); return TK_RIGHT_SHIFT; }
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
      case EOZ: {
        return TK_EOS;
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
              /* TODO: find the correct call for this */
              luaG_runerror(ls->H,
                "The reserved words '%s' and '%s' can "
                "only be used when the virtual machine is built with "
                "structure support.",
                luaX_tokens[TK_HMAKE-FIRST_RESERVED],
                luaX_tokens[TK_HSTRUCTURE-FIRST_RESERVED]);
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

