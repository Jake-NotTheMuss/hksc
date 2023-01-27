/*
** $Id: hksc.c $
** Lua compiler (saves bytecodes to files; also lists bytecodes)
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#define hksc_c

#include "hksclua.h"

#include "hksclib.h"

#include "hkscaux.h" /* auxiliary functions for standalone program */


#define HKSC_NAME "hksc" /* default program name */

#define HKSC_VERSION "0.0"

static int listing=0;     /* list bytecodes? */
static int dumping=1;     /* dump bytecodes? */

static int mode=HKSC_MODE_DEFAULT; /* compiling or decompiling? */

/* parser settings */
static int striplevel=BYTECODE_STRIPPING_NONE; /* bytecode stripping level */
static int literals_enabled=INT_LITERALS_NONE; /* int literal options */
static const char *progname=HKSC_NAME;
const char *output=NULL;

static int withdebug=0;

#ifdef LUA_COD
const char *debugfile=NULL;
const char *callstackdb=NULL;
int debugfile_arg=0;
int callstackdb_arg=0;
#else
static int ignore_debug=0;
#endif /* LUA_COD */

static void fatal(const char *message)
{
  fprintf(stderr,"%s: %s\n",progname,message);
  exit(EXIT_FAILURE);
}

#define error_multiple_inputs(opt) \
  usage("'" opt "' used with multiple input files")

#define warn_unused(opt,what) \
  fputs("warning: option '" opt "' is ignored when " what "\n", stderr)

static void print_usage(void)
{
  fprintf(stderr, "usage: %s [options] [filenames]\n", progname);
  fputs(
   "\nOperation modes:\n"
   "      --help              Print this message and exit\n"
   "      --version           Show version information\n"
   "      --printconfig       Print Hksc configuration\n"
#ifdef HKSC_DECOMPILER
   "  -c, --compile           Run Hksc in compile mode\n"
   "  -d, --decompile         Run Hksc in decompile mode\n"
#endif /* HKSC_DECOMPILER */
  , stderr);
  fputs(
   "\nCompiler options:\n"
   "  -L[=TYPE]               Enable int literals of the given TYPE\n"
#ifndef LUA_COD
   "  -s[=MODE]               Use bytecode stripping level MODE\n"
   "  -i, --ignoredebug       Ignore debug info when decompiling\n"
#endif /* LUA_COD */
   , stderr);
  fputs(
   "\nInput/Output options:\n"
   "  -o, --output=NAME       Output to file NAME\n"
   "  -p                      Parse only\n"
#ifndef LUA_COD
   "  -r, --withdebug         Load/dump debug information\n"
#else
   "  -r, --withdebug         Load/dump debug files with input/output files\n"
   "  -a, --callstackdb=FILE  Use FILE for callstack reconstruction\n"
   "  -g, --debugfile=FILE    Use FILE for debug info\n"
#endif /* LUA_COD */
   , stderr);
  fputs(
   "  --                      Stop handling options\n", stderr);
  fputs(
   "\nInt literal options for TYPE (to use with '-L')\n"
   "  32  Enable 32-bit int literals\n"
   "  64  Enable 64-bit int literals\n", stderr);
#ifndef LUA_COD /* use special arguments for cod */
  fputs(
   "\nBytecode stripping options for MODE (to use with '-s'):\n"
   "  n   Include all debug information in dump\n"
   "  p   Include profiling information in dump\n"
   , stderr);
#endif /* !LUA_COD */
}

static void usage(const char *message)
{
  if (*message == '-') {
    fprintf(stderr,"%s: unrecognized option '%s'\n",progname,message);
    print_usage();
  }
  else
    fprintf(stderr,"%s: %s\n",progname,message);
  exit(EXIT_FAILURE);
}

static void print_version(void)
{
  printf(HKSC_NAME " " HKSC_VERSION "\n" /* Hksc version */
#ifdef LUA_COD
    "Call of Duty "
#endif
    "Havok Script compiler"
#ifdef HKSC_DECOMPILER
    "/decompiler"
#endif
    "\n" LUA_VERSION " " LUA_COPYRIGHT "\n" /* Lua version */
  );
}


#define PRINT_YESNO(s,x) \
  fprintf(stdout, "  " s "            %s\n", ((HKSC_##x) ? "YES" : "NO"))

static void print_config(void)
{
  fputs("Compiler build settings:\n", stdout);
  PRINT_YESNO("MEMOIZATION", GETGLOBAL_MEMOIZATION);
  PRINT_YESNO("STRUCTURES ", STRUCTURE_EXTENSION_ON);
  PRINT_YESNO("SELF       ", SELF);
  PRINT_YESNO("DOUBLES    ", WITHDOUBLES);
  PRINT_YESNO("NATIVE INT ", WITHNATIVEINT);
}

#define IS(s) (strcmp(argv[i],s)==0)
#define HAS(s) (strncmp(argv[i],"" s,sizeof(s)-1)==0)

#define DOSTRINGARG(s,l,v) do { \
  char *val; \
  int shrt = IS(s); \
  if (shrt) val = argv[++i]; \
  else { \
    val = argv[i] + sizeof(l)-1; \
    if (*val == '=') val++; \
    else if (*val == '\0') val = argv[++i]; \
    else usage(argv[i]); \
  } \
  if (val == NULL || *val == '\0') { \
    if (shrt) usage("'" s "' needs an argument"); \
    else usage("'" l "' needs an argument"); \
  } \
  v = (const char *)val; \
} while (0)

#define ELSE_IF_STRING(s,l,v) else if (IS(s) || HAS(l)) DOSTRINGARG(s,l,v)

static int doargs(int argc, char *argv[])
{
  int i;
  int nfiles=0;
  int striparg=0;
  int version=0;
  int info=0;
#ifdef HKSC_DECOMPILER
  int c=0,d=0; /* uses of `-c' and `-d' */
#endif /* HKSC_DECOMPILER */
  if (argv[0]!=NULL && *argv[0]!=0) progname=argv[0];
  for (i=1; i<argc; i++)
  {
    if (*argv[i]!='-')      /* input file */
    {
      char *tmp = argv[++nfiles]; /* push names to the front */
      argv[nfiles] = argv[i];
      argv[i] = tmp;
    }
    else if (IS("--"))      /* end of options; skip it */
    {
      ++i;
      if (version) ++version;
      break;
    }
#if 0
    else if (IS("-"))     /* end of options; use stdin */
      break;
#endif
#ifdef HKSC_DECOMPILER
    else if (IS("-c") || IS("--compile")) ++c; /* specifies compile mode */
    else if (IS("-d") || IS("--decompile")) ++d; /* specified decompile mode */
#endif /* HKSC_DECOMPILER */
    else if (IS("-r") || IS("--withdebug")) withdebug=1;
#ifdef LUA_COD
    ELSE_IF_STRING("-a", "--callstackdb", callstackdb);
    ELSE_IF_STRING("-g", "--debugfile", debugfile);
#else
    else if (IS("-i") || IS("--ignoredebug")) ignore_debug=1;
#endif /* LUA_COD */
    else if (IS("--printconfig")) ++info;
    else if (IS("--help")) { /* print help message and exit */
      print_usage();
      exit(EXIT_SUCCESS);
    }
    else if (IS("-l"))      /* list */
      ++listing;
    else if (HAS("-L"))   /* specify int literal options */
    {
      char *mode;
      if (argv[i][2] == 0) {
        literals_enabled=INT_LITERALS_ALL; /* default */
        continue;
      }
      mode = argv[i]+2;
      if (*mode == '=') mode++;
      if (mode[0] == '\0' || mode[1] == '\0' || mode[2] != '\0')
        goto badliteralarg;
      switch (*mode) {
        case '3':
          if (mode[1] != '2') goto badliteralarg;
          literals_enabled|=INT_LITERALS_LUD; break;
        case '6':
          if (mode[1] != '4') goto badliteralarg;
          literals_enabled|=INT_LITERALS_UI64; break;
        default:
          goto badliteralarg;
      }
      continue;
      badliteralarg:
      usage("invalid int literal type given with '-L'");
    }
    ELSE_IF_STRING("-o", "--output", output);
    else if (IS("-p"))      /* parse only */
      dumping=0;
    else if (HAS("-s"))     /* specify stripping level */
    {
#ifdef LUA_COD
      /* do nothing */
      (void)striplevel;
#else /* !LUA_COD */
      char *mode;
      if (striparg)
        usage("'-s' used multiple times");
      if (argv[i][2] == 0) {
        striplevel=BYTECODE_STRIPPING_ALL; /* default */
        striparg=1;
        continue;
      }
      mode = argv[i]+2;
      if (*mode == '=') mode++;
      if (mode[0] == '\0' || mode[1] != '\0')
        goto badstriparg;
      switch (*mode) {
        case 'N': case 'n':
          striplevel=BYTECODE_STRIPPING_NONE; break;
        case 'P': case 'p':
          striplevel=BYTECODE_STRIPPING_PROFILING; break;
        case 'A': case 'a':
          striplevel=BYTECODE_STRIPPING_ALL; break;
#if 0
        case 'D': case 'd':
          striplevel=BYTECODE_STRIPPING_DEBUG_ONLY; break;
        case 'C': case 'c':
          striplevel=BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION; break;
#endif
        default:
          goto badstriparg;
      }
      striparg=1;
      continue;
      badstriparg:
      usage("invalid stripping mode given with '-s'");
#endif /* LUA_COD */
    }
    else if (IS("--version"))     /* show version */
      ++version;
    else
      usage(argv[i]);
  }
  if (version || info)
  {
    if (version) print_version();
    if (info) {
      if (version) fputc('\n', stdout);
      print_config();
    }
    exit(EXIT_SUCCESS);
  }
#ifdef HKSC_DECOMPILER
  if (c && d) /* both compile and decompile mode specified? */
    usage("both '-c' and '-d' used; Hksc can only be run in one mode");
  else if (c) {
#ifndef LUA_COD
    if (ignore_debug)
      warn_unused("--ignoredebug", "compiling");
#endif /* !LUA_COD */
    mode=HKSC_MODE_COMPILE;
  }
  else if (d)
    mode=HKSC_MODE_DECOMPILE;
#endif /* HKSC_DECOMPILER */
  if (striparg && (dumping
#ifdef HKSC_DECOMPILER
      || d
#endif
      ))
    warn_unused("-s", "not dumping bytecode");
  return nfiles;
}

#define FUNCTION "(function()end)();"

static hksc_DumpFunction dumpf;

/* dump function for -l */
static int hksc_dump_l(hksc_State *H, void *ud) {
  (void)ud;
  lua_print(H, listing > 1);
  return 0;
}

/* dump function for -p */
static int hksc_dump_p(hksc_State *H, void *ud) {
  (void)H;
  fprintf(stderr, "Successfully parsed `%s'\n", (const char *)ud);
  return 0;
}

/* default dump function */
static int hksc_dump_default(hksc_State *H, void *ud) {
  return hksc_dump_function(H, (const char *)ud);
}

/*
** parser loop function
*/
static int dofiles (hksc_State *H, int argc, char *argv[]) {
  int i, status, error = 0;
  for (i = 0; i < argc; i++) {
    error |= (status = hksI_parser_file(H, argv[i], dumpf, argv[i]));
    if (status) {
      if (status == LUA_ERRSYNTAX) {
        fprintf(stderr, "%s\n", lua_geterror(H));
        lua_clearerror(H); /* discharge the error message */
      } else {
        fprintf(stderr, "%s: %s\n", progname, lua_geterror(H));
        break; /* fatal */
      }
    }
  }
  return error;
}


int main(int argc, char *argv[])
{
  hksc_StateSettings settings;
  hksc_State *H;
  int status;
  int i=doargs(argc,argv);
  argc=i;argv+=1; /* in-files are pushed to front */
  if (argc<=0) usage("no input files given");
  /* warn about using -o with multiple input files */
  else if (argc > 1) {
    if (output != NULL)
      error_multiple_inputs("-o");
#ifdef LUA_COD
    if (debugfile != NULL)
      error_multiple_inputs("-g");
    if (callstackdb != NULL)
      error_multiple_inputs("-a");
#endif /* LUA_COD */
  }
  (void)settings;
  H = hksI_newstate(NULL);
  if (H==NULL) fatal("cannot create state: not enough memory");
  lua_setmode(H, mode);
  lua_setIntLiteralsEnabled(H,literals_enabled);
#ifdef LUA_COD
  if (dumping) {
    lua_onstartcycle(H, luacod_startcycle);
    lua_onendcycle(H, luacod_endcycle);
  }
  lua_setBytecodeStrippingLevel(H,BYTECODE_STRIPPING_ALL);
  debugfile_arg = (debugfile != NULL);
  callstackdb_arg = (callstackdb != NULL);
  withdebug = (withdebug || debugfile_arg || callstackdb_arg);
  lua_setIgnoreDebug(H, !withdebug);
#else /* !LUA_COD */
  lua_setBytecodeStrippingLevel(H,striplevel);
  lua_setIgnoreDebug(H, ignore_debug);
#endif /* LUA_COD */
  if (listing)
    dumpf = hksc_dump_l;
  else if (!dumping)
    dumpf = hksc_dump_p;
  else
    dumpf = hksc_dump_default;
  status = dofiles(H, argc, argv);
  hksI_close(H);
  return status ? EXIT_FAILURE : EXIT_SUCCESS;
}
