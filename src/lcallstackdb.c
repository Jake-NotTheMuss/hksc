/*
** $Id: lcallstackdb.c $
** Call of Duty callstackdb parser
** See Copyright Notice in lua.h
*/

#include <string.h>
#include <ctype.h>

#define lcallstackdb_c
#define LUA_CORE

#include "hksclua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "lzio.h"

#ifdef LUA_COD

enum CALLSTACKDB_LEXER_STATE {
  CALLSTACKDB_NONE,
  CALLSTACKDB_HASH,  /* hash field */
  CALLSTACKDB_INDEX,  /* index field */
  CALLSTACKDB_SOURCE,  /* source name field */
  CALLSTACKDB_LINE,  /* line number field */
  CALLSTACKDB_NAME,  /* function name field */
  CALLSTACKDB_NUMSTATES
};

typedef struct CallstackDBLexer {
  int current;  /* current character (charint) */
  int state;  /* the state of the lexer in the current CSV line */
  int linenumber;  /* input line counter */
  struct hksc_State *H;  /* copy of the Lua state */
  ZIO *z;  /* input stream */
  Mbuffer *buff;  /* buffer for tokens */
  Proto *f;  /* current function header */
  const char *name;  /* current callstackdb file name */
} CallstackDBLexer;


static void lexerror(CallstackDBLexer *ls, const char *message)
{
  luaD_setferror(ls->H, "%s:%d: %s", ls->name, ls->linenumber, message);
  luaD_throw(ls->H, LUA_ERRSYNTAX);
}


#define next(ls) (ls->current = zgetc(ls->z))

#define currIsNewline(ls) (ls->current == '\n' || ls->current == '\r')

#define save_and_next(ls) (save(ls, ls->current), next(ls))


static void error_expected (CallstackDBLexer *ls, int token) {
  lexerror(ls, luaO_pushfstring(ls->H, "'%c' expected", token));
}


static void check (CallstackDBLexer *ls, int c) {
  if (ls->current != c)
    error_expected(ls, c);
}


static void checknext (CallstackDBLexer *ls, int c) {
  check(ls, c);
  next(ls);
}


static void inclinenumber (CallstackDBLexer *ls) {
  int old = ls->current;
  lua_assert(currIsNewline(ls));
  next(ls);  /* skip `\n' or `\r' */
  if (currIsNewline(ls) && ls->current != old)
    next(ls);  /* skip `\n\r' or `\r\n' */
  if (++ls->linenumber >= MAX_INT)
    lexerror(ls, "file has too many lines");
}


static void save (CallstackDBLexer *ls, int c) {
  Mbuffer *b = ls->buff;
  if (b->n + 1 > b->buffsize) {
    size_t newsize;
    if (b->buffsize >= MAX_SIZET/2)
      lexerror(ls, "lexical element too long");
    newsize = b->buffsize * 2;
    luaZ_resizebuffer(ls->H, b, newsize);
  }
  b->buffer[b->n++] = cast(char, c);
}


static int parse_int(CallstackDBLexer *ls) {
  int val;
  char *s;
  char *endptr;
  if (!isdigit(ls->current))
    lexerror(ls, "bad integer value for field");
  luaZ_resetbuffer(ls->buff);
  while (isdigit(ls->current))
    save_and_next(ls);
  save(ls, '\0');
  s = luaZ_buffer(ls->buff);
  val = cast(int, strtoul(s, &endptr, 10));
  if (*endptr != '\0' || endptr == s)
    lexerror(ls, "invalid integer value");
  return val;
}


static lu_int32 parse_hash(CallstackDBLexer *ls) {
  lu_int32 hash;
  char *s;
  char *endptr;
  if (!isdigit(ls->current))
    lexerror(ls, "bad hash value for field");
  luaZ_resetbuffer(ls->buff);
  while (isdigit(ls->current))
    save_and_next(ls);
  save(ls, '\0');
  s = luaZ_buffer(ls->buff);
  hash = cast(lu_int32, strtoul(s, &endptr, 10));
  if (*endptr != '\0' || endptr == s)
    lexerror(ls, "invalid hash value");
  return hash;
}


static TString *parse_string(CallstackDBLexer *ls) {
  char *buffer;
  size_t len;
  luaZ_resetbuffer(ls->buff);
  while (ls->current != ',' && !currIsNewline(ls))
    save_and_next(ls);
  buffer = luaZ_buffer(ls->buff);
  len = luaZ_bufflen(ls->buff);
  if (len == 0)
    return NULL;
  else
    return luaS_newlstr(ls->H, buffer, len);
}


static lu_int32 parse_profile(CallstackDBLexer *ls, lu_int32 hash) {
  int expected_index = 0;
  int index = 0;
  if (ls->linenumber == 1) {
    next(ls);
    ls->state = CALLSTACKDB_HASH;
    hash = parse_hash(ls);
  }
  else if (ls->state == CALLSTACKDB_INDEX) {
    goto parsedindex;
  }
  ls->state = CALLSTACKDB_INDEX;
  while (hash == ls->f->hash) {
    TString *source, *name;
    int line;
    checknext(ls, ',');
    lua_assert(ls->state == CALLSTACKDB_INDEX);
    index = parse_int(ls);
    if (index != expected_index) {
      if (index == 0) /* maybe a collision */
        return hash;
      else
        lexerror(ls, "bad line-info index");
    }
    parsedindex:
    checknext(ls, ',');
    ls->state++;
    source = parse_string(ls);
    if (index == 0)
      ls->f->source = source;
    else {
      if ((source == NULL) != (ls->f->source == NULL))
        lexerror(ls, "bad source name");
      if (source != NULL && strcmp(getstr(source), getstr(ls->f->source)))
        lexerror(ls, "bad source name");
    }
    checknext(ls, ',');
    ls->state++;
    line = parse_int(ls);
    luaM_reallocvector(ls->H, ls->f->lineinfo, ls->f->sizelineinfo,
                       ls->f->sizelineinfo+1, int);
    ls->f->lineinfo[ls->f->sizelineinfo++] = line;
    checknext(ls, ',');
    ls->state++;
    name = parse_string(ls);
    if (index == 0)
      ls->f->name = name;
    else {
      if ((name == NULL) != (ls->f->name == NULL))
        lexerror(ls, "bad function name");
      if (name != NULL && strcmp(getstr(name), getstr(ls->f->name)))
        lexerror(ls, "bad function name");
    }
    if (!currIsNewline(ls))
      lexerror(ls, "unexpected characters at end of line");
    inclinenumber(ls);
    ls->state = CALLSTACKDB_HASH;
    hash = parse_hash(ls);
    expected_index++;
    ls->state++;
  }
  ls->state = CALLSTACKDB_NONE;
  return hash;
}


void luaU_parsecallstackdb (hksc_State *H, ZIO *z, Mbuffer *buff, Proto *f,
                            const char *name, CallstackDBLexState *savestate) {
  CallstackDBLexer ls;
  ls.H = H;
  ls.z = z;
  ls.buff = buff;
  ls.f = f;
  ls.name = name;
  ls.current = 0;
  ls.state = (savestate->lastline == 1)?CALLSTACKDB_NONE : savestate->laststate;
  ls.linenumber = savestate->lastline;
  savestate->lookahead_hash = parse_profile(&ls, savestate->lookahead_hash);
  savestate->lastline = ls.linenumber;
  savestate->laststate = ls.state;
}

#endif /* LUA_COD */
