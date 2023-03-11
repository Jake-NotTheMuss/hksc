/*
** $Id: llex.h $
** Lexical Analyzer
** See Copyright Notice in lua.h
*/

#ifndef llex_h
#define llex_h

#include "lobject.h"
#include "lzio.h"

enum TextModes {
  ASCII,
  UTF8
};


#define FIRST_RESERVED  0x400001

#define DEFTOKFIRST(name, text) name = FIRST_RESERVED,
#define DEFTOK(name, text) name,
enum RESERVED {
#include "ltoken.def"
  TK_LAST
};
#undef DEFTOKFIRST
#undef DEFTOK


/* maximum length of a reserved word */
#define DEFTOK(name, text) char buf_##name[sizeof(text)];
union max_token_length {
#include "ltoken.def"
};
#undef DEFTOK

#define TOKEN_LEN (sizeof(union max_token_length)/sizeof(char))

/* number of reserved words */
#define NUM_RESERVED  (cast(int, TK_RESERVED_LAST-FIRST_RESERVED))

/* array with token `names' */
LUAI_DATA const char *const luaX_tokens [];


typedef union {
  lua_Number r;
  lu_int64 l;
  TString *ts;
} SemInfo;  /* semantics information */


typedef struct Token {
  int token;
  SemInfo seminfo;
} Token;


/* defined in lparser.c; holds the name parts of the current function */
struct FunctionNameStack;


typedef struct LexState {
  int current;  /* current character (charint) */
  int linenumber;  /* input line counter */
  int lastline;  /* line of last token `consumed' */
  int textmode; /* text mode */
  Token t;  /* current token */
  Token lookahead;  /* look ahead token */
  struct FuncState *fs;  /* `FuncState' is private to the parser */
  struct hksc_State *H;
  struct FunctionNameStack *functionNameStack;
  ZIO *z;  /* input stream */
  Mbuffer *buff;  /* buffer for tokens */
  TString *source;  /* current source name */
  char decpoint;  /* locale decimal point */
} LexState;


LUAI_FUNC void luaX_init (hksc_State *H);
LUAI_FUNC void luaX_setinput (hksc_State *H, LexState *LS, ZIO *z,
                              TString *source);
LUAI_FUNC TString *luaX_newstring (LexState *LS, const char *str, size_t l);
LUAI_FUNC void luaX_next (LexState *ls);
LUAI_FUNC void luaX_lookahead (LexState *ls);
LUAI_FUNC void luaX_readFirstToken (LexState *ls);
LUAI_FUNC void luaX_lexerror (LexState *ls, const char *msg, int token);
LUAI_FUNC void luaX_syntaxerror (LexState *ls, const char *s);
LUAI_FUNC void luaX_inputerror (LexState *ls, const char *s);
LUAI_FUNC const char *luaX_token2str (LexState *ls, int token);


#endif
