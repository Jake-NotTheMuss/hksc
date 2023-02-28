/*
** $Id: ltests.c $
** Internal Module for Debugging of the Lua Implementation
** See Copyright Notice in lua.h
*/


#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ltests_c
#define LUA_CORE

#include "hksclua.h"
#include "hksclib.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "llog.h"
#include "lmem.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"



/*
** The whole module only makes sense with LUA_DEBUG on
*/
#if defined(LUA_DEBUG)


int Trick = 0;


static hksc_State *lua_state = NULL;

int islocked = 0;


/*
** Logging stuff for tests
*/
#ifdef HKSC_LOGGING

/* extra code for logging test information */
#define LOG_PRIORITY_INTERNAL LOG_PRIORITY_MAX

#if defined(__unix__) || defined(__APPLE__) || defined(__HAIKU__)
# include <unistd.h>
# define lua_is_tty(f)  isatty(fileno(f))
#else
# define lua_is_tty(f)  0
#endif

/*
** Colors to map to log priority levels
*/
#define COLOR_BLACK    0
#define COLOR_RED      1
#define COLOR_GREEN    2
#define COLOR_YELLOW   3
#define COLOR_BLUE     4
#define COLOR_MAGENTA  5
#define COLOR_CYAN     6
#define COLOR_WHITE    7
#define COLOR_RESET    8

#define BOLDCODE "\033[1m"

#define COLOR_DEFAULT COLOR_WHITE

static const char *const color_table[] = {
  "\033[30m",
  "\033[31m",
  "\033[32m",
  "\033[33m",
  "\033[34m",
  "\033[35m",
  "\033[36m",
  "\033[0m",
  "\033[0m"
};

/* set what colors you want for each priority level */
static int priority_colors[] = {
  [LOG_PRIORITY_DEBUG] = COLOR_GREEN,
  [LOG_PRIORITY_INFO] = COLOR_DEFAULT,
  [LOG_PRIORITY_WARN] = COLOR_YELLOW,
  [LOG_PRIORITY_ERROR] = COLOR_RED,
  [LOG_PRIORITY_FATAL] = COLOR_RED,
  [LOG_PRIORITY_INTERNAL] = COLOR_CYAN
};

static int use_color;

static int debug_log(hksc_State *H, const char *label, int priority,
                     const char *msg, void *ud) {
  FILE *f;
  UNUSED(H);
  f = (FILE *)ud;
  if (use_color)
    fprintf(f, BOLDCODE);
    /* print label */
  fprintf(f, "[%s]", label);
  if (use_color) {
    fprintf(f, "%s", color_table[COLOR_RESET]);
    /* switch to color for priority */
    fprintf(f, "%s", color_table[priority_colors[priority]]);
  }
  /* print msg */
  fprintf(f, " %s\n", msg);
  if (use_color)
    /* reset color */
    fprintf(f, "%s", color_table[COLOR_RESET]);
  return 0;
}

#endif /* HKSC_LOGGING */

hksc_State *debug_newstate(hksc_StateSettings *settings)
{
  hksc_State *H;
  hksc_StateSettings default_settings;
  if (!settings) {
    hksI_StateSettings(&default_settings);
    settings = &default_settings;
  }
  settings->frealloc = debug_realloc;
  settings->ud = &memcontrol;
#ifdef HKSC_LOGGING
  settings->logctx.f = &debug_log;
  if (settings->logctx.ud == NULL)
    settings->logctx.ud = stderr;
  settings->logctx.priority = LOG_PRIORITY_DEBUG;
#endif /* HKSC_LOGGING */
  H = lua_newstate(settings);
  if (H) {
#ifdef HKSC_LOGGING
    use_color = lua_is_tty(settings->logctx.ud);
#endif /* HKSC_LOGGING */
    lua_lock(H);
    lua_loginfo(H,        "created a new Lua debug state");
    lua_loginfo(H,         "--------------------------------------");
    lua_logdebug(H,        "This is what DEBUG messages look like.");
    lua_loginfo(H,         "This is what INFO messages look like.");
    lua_logwarning(H,      "This is what WARN messages look like.");
    lua_logerror(H,        "This is what ERROR messages look like.");
    lua_logfatalerror(H,   "This is what FATAL messages look like.");
    lua_logtest(H,         "This is what TEST messages look like.");
    lua_loginfo(H,         "--------------------------------------");
    lua_unlock(H);
    luaB_opentests(H);
  }
  return H;
}


/*
** {======================================================================
** Controlled version for realloc.
** =======================================================================
*/

#define MARK    0x55  /* 01010101 (a nice pattern) */

#ifndef EXTERNMEMCHECK
/* full memory check */
#define HEADER  (sizeof(L_Umaxalign)) /* ensures maximum alignment for HEADER */
#define MARKSIZE  16  /* size of marks after each block */
#define blockhead(b)  (cast(char *, b) - HEADER)
#define setsize(newblock, size) (*cast(size_t *, newblock) = size)
#define checkblocksize(b, size) (size == (*cast(size_t *, blockhead(b))))
#define fillmem(mem,size) memset(mem, -MARK, size)
#else
/* external memory check: don't do it twice */
#define HEADER    0
#define MARKSIZE  0
#define blockhead(b)  (b)
#define setsize(newblock, size) /* empty */
#define checkblocksize(b,size)  (1)
#define fillmem(mem,size) /* empty */
#endif


Memcontrol memcontrol = {0L, 0L, 0L, 0L};


static void *checkblock (void *block, size_t size) {
  void *b = blockhead(block);
  int i;
  for (i=0;i<MARKSIZE;i++)
    lua_assert(*(cast(char *, b)+HEADER+size+i) == MARK+i); /* corrupted block? */
  return b;
}


static void freeblock (Memcontrol *mc, void *block, size_t size) {
  if (block) {
    lua_assert(checkblocksize(block, size));
    block = checkblock(block, size);
    fillmem(block, size+HEADER+MARKSIZE);  /* erase block */
    free(block);  /* free original block */
    mc->numblocks--;
    mc->total -= size;
  }
}


void *debug_realloc (void *ud, void *block, size_t oldsize, size_t size) {
  Memcontrol *mc = cast(Memcontrol *, ud);
  lua_assert(oldsize == 0 || checkblocksize(block, oldsize));
  if (mc->memlimit == 0) {  /* first time? */
    char *limit = getenv("MEMLIMIT");  /* initialize memory limit */
    mc->memlimit = limit ? strtoul(limit, NULL, 10) : ULONG_MAX;
  }
  if (size == 0) {
    freeblock(mc, block, oldsize);
    return NULL;
  }
  else if (size > oldsize && mc->total+size-oldsize > mc->memlimit)
    return NULL;  /* to test memory allocation errors */
  else {
    void *newblock;
    int i;
    size_t realsize = HEADER+size+MARKSIZE;
    size_t commonsize = (oldsize < size) ? oldsize : size;
    if (realsize < size) return NULL;  /* overflow! */
    newblock = malloc(realsize);  /* alloc a new block */
    if (newblock == NULL) return NULL;
    if (block) {
      memcpy(cast(char *, newblock)+HEADER, block, commonsize);
      freeblock(mc, block, oldsize);  /* erase (and check) old copy */
    }
    /* initialize new part of the block with something `weird' */
    fillmem(cast(char *, newblock)+HEADER+commonsize, size-commonsize);
    mc->total += size;
    if (mc->total > mc->maxmem)
      mc->maxmem = mc->total;
    mc->numblocks++;
    setsize(newblock, size);
    for (i=0;i<MARKSIZE;i++)
      *(cast(char *, newblock)+HEADER+size+i) = cast(char, MARK+i);
    return cast(char *, newblock)+HEADER;
  }
}


/* }====================================================================== */


const char *const luaT_typenames[] = {
  "nil", "boolean", "userdata", "number",
  "string", "table", "function", "userdata", "thread",
  "ifunction", "cfunction", "ui64", "struct", "",
  "proto", "upval"
};


/*
** {======================================================
** Functions to check memory consistency
** =======================================================
*/

static int testobjref1 (global_State *g, GCObject *f, GCObject *t) {
  UNUSED(f);
  if (isdead(g,t)) return 0;
  else
    return 1;
}


static void printobj (global_State *g, GCObject *o) {
  int i = 0;
  GCObject *p;
  for (p = g->rootgc; p != o && p != NULL; p = p->gch.next) i++;
  if (p == NULL) i = -1;
  printf("%d:%s(%p)-%c(%02X)", i, luaT_typenames[o->gch.tt], (void *)o,
           istemp(g,o)?'t':isdead(g,o)?'b':'w', o->gch.marked);
}


static int testobjref (global_State *g, GCObject *f, GCObject *t) {
  int r = testobjref1(g,f,t);
  if (!r) {
    printf("%d(%02X) - ", g->gcstate, g->currentwhite);
    printobj(g, f);
    printf("\t-> ");
    printobj(g, t);
    printf("\n");
  }
  return r;
}

#define checkobjref(g,f,t) lua_assert(testobjref(g,f,obj2gco(t)))

#define checkvalref(g,f,t) lua_assert(!iscollectable(t) || \
  ((ttype(t) == (t)->value.gc->gch.tt) && testobjref(g,f,gcvalue(t))))



static void checktable (global_State *g, Table *h) {
  int i;
  GCObject *hgc = obj2gco(h);
  if (h->metatable)
    checkobjref(g, hgc, h->metatable);
  i = h->sizearray;
  while (i--)
    checkvalref(g, hgc, &h->array[i]);
  i = sizenode(h);
  while (i--) {
    Node *n = gnode(h, i);
    if (!ttisnil(gval(n))) {
      lua_assert(!ttisnil(gkey(n)));
      checkvalref(g, hgc, gkey(n));
      checkvalref(g, hgc, gval(n));
    }
  }
}


/*
** All marks are conditional because a GC may happen while the
** prototype is still being created
*/
static void checkproto (global_State *g, Proto *f) {
  int i;
  GCObject *fgc = obj2gco(f);
  if (f->source) checkobjref(g, fgc, f->source);
  for (i=0; i<f->sizek; i++) {
    if (ttisstring(f->k+i))
      checkobjref(g, fgc, rawtsvalue(f->k+i));
  }
  for (i=0; i<f->sizeupvalues; i++) {
    if (f->upvalues[i])
      checkobjref(g, fgc, f->upvalues[i]);
  }
  for (i=0; i<f->sizep; i++) {
    if (f->p[i])
      checkobjref(g, fgc, f->p[i]);
  }
  for (i=0; i<f->sizelocvars; i++) {
    if (f->locvars[i].varname)
      checkobjref(g, fgc, f->locvars[i].varname);
  }
}



static void checkobject (global_State *g, GCObject *o) {
  if (isdead(g, o))
/*    lua_assert(g->gcstate == GCSsweepstring || g->gcstate == GCSsweep);*/
{ if (!(g->gcstate == GCSsweepstring || g->gcstate == GCSsweep))
printf(">>> %d  %s  %02x\n", g->gcstate, luaT_typenames[o->gch.tt], o->gch.marked);
}
  else {
    /*if (g->gcstate == GCSfinalize)
      lua_assert(iswhite(o));*/
    switch (o->gch.tt) {
      case LUA_TTABLE: {
        checktable(g, gco2h(o));
        break;
      }
      case LUA_TPROTO: {
        checkproto(g, gco2p(o));
        break;
      }
      default: lua_assert(0);
    }
  }
}


int lua_checkmemory (hksc_State *H) {
  global_State *g = G(H);
  GCObject *o;
  for (o = g->rootgc; o != obj2gco(g->mainthread); o = o->gch.next)
    checkobject(g, o);
  for (o = o->gch.next; o != NULL; o = o->gch.next) {
    lua_assert(o->gch.tt == LUA_TUSERDATA);
    checkobject(g, o);
  }
  return 0;
}

/* }====================================================== */



/*
** {======================================================
** Disassembler
** =======================================================
*/


static char *buildop (Proto *p, int pc, char *buff) {
  Instruction i = p->code[pc];
  OpCode o = GET_OPCODE(i);
  const char *name = luaP_opnames[o];
  int line = getline(p, pc);
  sprintf(buff, "(%4d) %4d - ", line, pc);
  switch (getOpMode(o)) {  
    case iABC:
      sprintf(buff+strlen(buff), "%-12s%4d %4d %4d", name,
              GETARG_A(i), GETARG_B(i), GETARG_C(i));
      break;
    case iABx:
      sprintf(buff+strlen(buff), "%-12s%4d %4d", name, GETARG_A(i), GETARG_Bx(i));
      break;
    case iAsBx:
      sprintf(buff+strlen(buff), "%-12s%4d %4d", name, GETARG_A(i), GETARG_sBx(i));
      break;
  }
  return buff;
}


#if 1
void luaI_printcode (Proto *pt, int size) {
  int pc;
  for (pc=0; pc<size; pc++) {
    char buff[100];
    printf("%s\n", buildop(pt, pc, buff));
  }
  printf("-------\n");
}
#endif


/* }====================================================== */



static void checkfinalmem (void) {
  lua_assert(memcontrol.numblocks == 0);
  lua_assert(memcontrol.total == 0);
}


static const char *currentcheck=NULL;


#define lua_checkstarted(H) \
  (lua_loginfo(H, ""), /* print empty line */ \
   lua_logtest(H,   "----------- STARTING CHECK -----------"), \
   lua_logtest(H, luaI_formatmsg(H, "STARTING CHECK: %s", currentcheck)))
#define lua_checkpassed(H) \
  (lua_logtest(H,   "------------ CHECK PASSED ------------"), \
   lua_loginfo(H, "")/* print empty line */)
#define lua_checkfailed(H) \
  (lua_logerror(H,  "------------ CHECK FAILED ------------"), \
   lua_loginfo(H, "")/* print empty line */)

/* an even more debug-friendly assert which logs the assertion failure */
#define test_assert(H,c) do { \
  if (!(c)) { \
    lua_logerror(H, luaI_formatmsg(H, \
      "%s:%d: assertion failed: (%s), (in function %s)", __FILE__, __LINE__, \
      #c, currentcheck)); \
    lua_assert(c); \
    return 1; \
  } \
} while (0)

#define test_api_assert(H,c) do { \
  int cond = (c); \
  if (!cond) { \
    lua_lock(H); \
    lua_logerror(H, luaI_formatmsg(H, \
      "%s:%d: assertion failed: (%s), (in function %s)", __FILE__, __LINE__, \
      #c, currentcheck)); \
    lua_unlock(H); \
    lua_assert(c); \
    return 1; \
  } \
} while (0)

static void lua_docheck(hksc_State *H, lua_CFunction f, const char *name) {
  int status;
  currentcheck = name;
  lua_lock(H);
  lua_checkstarted(H);
  status = (*f)(H);
  if (status)
    lua_checkfailed(H);
  else
    lua_checkpassed(H);
  lua_unlock(H);
}

#define docheck(H,f) lua_docheck(H, f, #f)

#define lua_checking(f) do { \
  lua_lock(H); lua_logtest(H, "Checking `" #f "'"); lua_unlock(H); \
} while (0)


static void f_pcalldummy(hksc_State *H, void *ud) {
  UNUSED(ud);
  hksc_seterror(H, "HKSC TEST ERROR");
  luaD_throw(H, LUA_ERRERR);
}

#ifdef HKSC_LOGGING
static int dummy_log(hksc_State *H, const char *label, int priority,
                     const char *msg, void *ud) {
  (void)ud;
  return debug_log(H,label,priority,msg,stderr);
}
#endif /* HKSC_LOGGING */


/*
** check APIs that aren't used by the standalone compiler
*/
static int apichecks(hksc_State *H) {
  int dummy;
  int mode;
  lua_CFunction panic;
  hksc_CycleCallback cycle;
#ifdef HKSC_LOGGING
  hksc_LogFunction log;
  hksc_LogFunction dummylog;
  void *ud = NULL;
  void *dummyud = NULL;
  int logpriority;
#endif /* HKSC_LOGGING */
#if defined(LUA_COD)
  const char *debugfile = NULL;
  int debugfiletype = LUA_COD_DEBUG_NONE;
#endif /* defined(LUA_COD) && defined(HKSC_DECOMPILER) */
  const char *string = NULL;
  /* lua_atpanic */
  lua_checking(lua_atpanic);
  panic = lua_atpanic(H, NULL);
  test_api_assert(H, lua_atpanic(H, panic) == NULL);
  test_api_assert(H, lua_atpanic(H, panic) == panic);
  /* lua_onstart_cycle */
  lua_checking(lua_onstartcycle);
  cycle = lua_onstartcycle(H, NULL);
  test_api_assert(H, lua_onstartcycle(H, cycle) == NULL);
  test_api_assert(H, lua_onstartcycle(H, cycle) == cycle);
  /* lua_onendcycle */
  lua_checking(lua_onendcycle);
  cycle = lua_onendcycle(H, NULL);
  test_api_assert(H, lua_onendcycle(H, cycle) == NULL);
  test_api_assert(H, lua_onendcycle(H, cycle) == cycle);
  /* lua_newstring */
  lua_checking(lua_newstring);
  string = lua_newstring(H, "HELLO");
  test_api_assert(H, strcmp(string, "HELLO") == 0);
  /* lua_newlstring */
  lua_checking(lua_newlstring);
  string = lua_newlstring(H, "HELLO", 3);
  test_api_assert(H, strcmp(string, "HEL") == 0);
  /* lua_newfstring/lua_newvfstring */
  lua_checking(lua_newfstring);
  string = lua_newfstring(H, "HELLO%s%d%u%c%%", "WORLD", 1, 2, 'a');
  test_api_assert(H, strcmp(string, "HELLOWORLD12a%") == 0);
  /* lua_geterror */
  lua_checking(lua_geterror);
  test_api_assert(H, lua_geterror(H) == NULL);
  dummy = luaD_pcall(H, f_pcalldummy, NULL);
  test_api_assert(H, lua_geterror(H) != NULL);
  /* lua_seterror */
  lua_checking(lua_seterror);
  lua_seterror(H, "error set by lua_seterror");
  string = lua_geterror(H);
  test_api_assert(H, strcmp(lua_geterror(H), "error set by lua_seterror") == 0);
  /* lua_setferror/lua_setvferror */
  lua_checking(lua_setferror);
  lua_setferror(H, "error code %d: %s", 1, "out of memory");
  test_api_assert(H, strcmp(lua_geterror(H),
                            "error code 1: out of memory") == 0);
  /* lua_clearerror */
  lua_checking(lua_clearerror);
  test_api_assert(H, lua_geterror(H) != NULL);
  lua_clearerror(H);
  test_api_assert(H, lua_geterror(H) == NULL);
  /* lua_getmode */
  lua_checking(lua_getmode);
  mode = hksc_mode(H);
  test_api_assert(H, mode == lua_getmode(H));
  /* lua_setmode */
  lua_checking(lua_setmode);
  /* change whatever value it was */
  mode = (mode == HKSC_MODE_DEFAULT) ? HKSC_MODE_SOURCE : HKSC_MODE_DEFAULT;
  lua_setmode(H, mode);
  test_api_assert(H, lua_getmode(H) == mode);
#ifdef HKSC_LOGGING
  /* lua_getlogf */
  lua_checking(lua_getlogf);
  log = lua_getlogf(H, &ud);
  test_api_assert(H, log == &debug_log);
  test_api_assert(H, ud == (void *)stderr);
  /* lua_setlogf */
  lua_checking(lua_setlogf);
  lua_setlogf(H, &dummy_log, &dummy);
  dummylog = lua_getlogf(H, &dummyud);
  test_api_assert(H, dummylog == &dummy_log);
  test_api_assert(H, dummyud == &dummy);
  /* clean up */
  lua_setlogf(H, log, ud);
  /* lua_getlogpriority */
  lua_checking(lua_getlogpriority);
  logpriority = G(H)->logctx.priority;
  test_api_assert(H, logpriority == lua_getlogpriority(H));
  /* lua_setlogpriority */
  lua_checking(lua_setlogpriority);
  /* change whatever value it was */
  logpriority = (logpriority == LOG_PRIORITY_DEBUG) ? LOG_PRIORITY_INFO : 
                                                     LOG_PRIORITY_DEBUG;
  lua_setlogpriority(H, logpriority);
  test_api_assert(H, logpriority == lua_getlogpriority(H));
#endif /* HKSC_LOGGING */
#if defined(LUA_COD) && defined(HKSC_DECOMPILER)
  /* lua_getDebugFile */
  lua_checking(lua_getDebugFile);
  debugfile = H->currdebugfile;
  H->currdebugfile = "TESTDEBUGFILE";
  debugfiletype = H->currdebugfiletype;
  H->currdebugfiletype = debugfiletype + 1;
  test_api_assert(H, lua_getDebugFile(H, &debugfiletype) == H->currdebugfile &&
                     strcmp(lua_getDebugFile(H, NULL), "TESTDEBUGFILE") == 0 &&
                     debugfiletype == H->currdebugfiletype);
  /* lua_setDebugFile */
  lua_checking(lua_setDebugFile);
  lua_setDebugFile(H, debugfile, debugfiletype);
  test_api_assert(H, lua_getDebugFile(H, &debugfiletype) == debugfile &&
                     H->currdebugfiletype == debugfiletype);
#endif /* defined(LUA_COD) && defined(HKSC_DECOMPILER) */
  return 0;
}

static void lua_doapichecks(hksc_State *H) {
  int status;
  lua_lock(H);
  lua_logtest(H, "");
  lua_logtest(H, "Starting Lua API checks");
  lua_logtest(H, "--------------------------------------");
  lua_unlock(H);
  status = apichecks(H);
  lua_lock(H);
  if (status) 
    lua_checkfailed(H);
  else
    lua_checkpassed(H);
  lua_unlock(H);
}

/*
** make sure `luaO_pushfstring' behaves as expected
*/
static int checkpushfstring(hksc_State *H) {
  int result;
  const char *str;
  char pcnt = '\0'; /* '%' read in sscanf */
  char c = 'z';
  int i = 100;
  unsigned int u = 1488553344;
  lua_Number f = 100.0f;
  void *p = NULL;  /* H is not NULL */
  char s[sizeof("hello")+100] = {0};
  lua_logtest(H, "testing functionality of `luaO_pushfstring'");
  lua_logtest(H, "invokation: `luaO_pushfstring(H, \"%c %d %u %f %p %s %%\","
               "'a', 1, 2, 3.0f, (void *)H, \"hello\");");
  str = luaO_pushfstring(H, "%c %d %u %f %p %s %%",
                         'a', 101, 1488553398, 3.0f, cast(void *, H), "hello");
  lua_logtest(H, luaI_formatmsg(H, "returned: \"%s\"", str));
  result = sscanf(str,"%c %d %u %f %p %[a-z] %c", &c, &i, &u, &f, &p, s, &pcnt);
  test_assert(H, result == 7);
  test_assert(H, c == 'a');
  test_assert(H, i == 101);
  test_assert(H, u == 1488553398);
  test_assert(H, f > 2.9f && f < 3.1f);
  test_assert(H, p == cast(void *, H));
  test_assert(H, strcmp(s, "hello") == 0);
  test_assert(H, pcnt == '%');
  return 0;
}


void luaB_opentests (hksc_State *H) {
  void *ud;
  atexit(checkfinalmem);
  lua_assert(lua_getallocf(H, &ud) == debug_realloc);
  lua_assert(ud == cast(void *, &memcontrol));
  lua_setallocf(H, lua_getallocf(H, NULL), ud);
  lua_state = H;  /* keep first state to be opened */
  /* internal tests */
  docheck(H, checkpushfstring);
  /* API tests */
  lua_doapichecks(H);
}

#endif
