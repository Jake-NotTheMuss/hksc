/*
** $Id: llog.h $
** Logging information and warnings
** See Copyright Notice in lua.h
*/

#ifndef llog_h
#define llog_h

#ifdef HKSC_LOGGING

#define LOG_CATEGORY_TABLE \
  DEFLOGCATEGORY(ANALYZER       , "ANALYZER CORE") \
  DEFLOGCATEGORY(API            , "LUA API") \
  DEFLOGCATEGORY(CODE           , "CODE GENERATOR") \
  DEFLOGPRIORITY(DEBUG          , "DEBUG CORE") \
  DEFLOGCATEGORY(DECOMPILER     , "DECOMPILER") \
  DEFLOGCATEGORY(DO             , "EXCEPTION CORE") \
  DEFLOGCATEGORY(DUMP           , "BYTECODE WRITER") \
  DEFLOGCATEGORY(FUNC           , "FUNCTION CORE") \
  DEFLOGCATEGORY(GC             , "GARBAGE COLLECTOR") \
  DEFLOGCATEGORY(LEX            , "LEXER") \
  DEFLOGCATEGORY(LOG            , "LOGGER") \
  DEFLOGCATEGORY(MEM            , "MEMORY") \
  DEFLOGCATEGORY(OBJECT         , "OBJECT CORE") \
  DEFLOGCATEGORY(OPCODES        , "OPCODE CORE") \
  DEFLOGCATEGORY(PARSER         , "PARSER") \
  DEFLOGCATEGORY(STATE          , "STATE CORE") \
  DEFLOGCATEGORY(STRING         , "STRING CORE") \
  DEFLOGCATEGORY(TABLE          , "TABLE CORE") \
  DEFLOGCATEGORY(TEST           , "TEST CORE") \
  DEFLOGCATEGORY(UNDUMP         , "BYTECODE LOADER") \
  DEFLOGCATEGORY(ZIO            , "STREAM CORE")

#define DEFLOGCATEGORY(e,n) LOG_CATEGORY_ ## e,
enum LOG_CATEGORY {
  LOG_CATEGORY_TABLE
  LOG_CATEGORY_MAX
};
#undef DEFLOGCATEGORY

/*#define DEFLOGCATEGORY(e,n) char buf_##e[sizeof(n)];
union max_label_length {
  LOG_CATEGORY_TABLE
};
#undef DEFLOGCATEGORY

#define MAX_LABEL_LEN (sizeof(union max_label_length)/sizeof(char))*/

#if defined(lanalyzer_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_ANALYZER
#elif defined(lapi_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_API
#elif defined(lcode_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_CODE
#elif defined(ldebug_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_DEBUG
#elif defined(ldo_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_DO
#elif defined(ldump_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_DUMP
#elif defined(lfunc_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_FUNC
#elif defined(lgc_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_GC
#elif defined(llex_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_LEX
#elif defined(llog_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_LOG
#elif defined(lmem_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_MEM
#elif defined(lobject_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_OBJECT
#elif defined(lopcodes_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_OPCODES
#elif defined(lparser_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_PARSER
#elif defined(lstate_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_STATE
#elif defined(lstring_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_STRING
#elif defined(ltable_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_TABLE
#elif defined(ltest_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_TEST
#elif defined(lundump_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_UNDUMP
#elif defined(lzio_c)
# define CURRENT_LOG_CATEGORY LOG_CATEGORY_ZIO
#else
# error You need to add the current translation unit to llog.h
#endif

LUAI_FUNC void luaI_log(hksc_State *H, int category, int priority,
                        const char *msg);
LUAI_FUNC void luaI_formatmsg(hksc_State *H, const char *fmt, ...);

#define lua_log(H,p,msg)  luaI_log(H,CURRENT_LOG_CATEGORY,p,msg)

#else /* !HKSC_LOGGING */

#define lua_log(H,p,msg)  ((void)0)

#endif /* HKSC_LOGGING */

#define lua_logdebug(H,msg)  lua_log(H,LOG_PRIORITY_DEBUG,msg)
#define lua_loginfo(H,msg)  lua_log(H,LOG_PRIORITY_INFO,msg)
#define lua_logwarning(H,msg)  lua_log(H,LOG_PRIORITY_WARN,msg)
#define lua_logerror(H,msg)  lua_log(H,LOG_PRIORITY_ERROR,msg)
#define lua_logfatalerror(H,msg)  lua_log(H,LOG_PRIORITY_FATAL,msg)

#endif
