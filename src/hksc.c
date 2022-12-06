/*
** $Id: hksc.c $
** Lua compiler (saves bytecodes to files; also lists bytecodes)
** See Copyright Notice in lua.h
*/

#define hksc_c
#define LUA_CORE

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "luaconf.h"

#include "hksclib.h"

#include "lctype.h"
#include "lundump.h"

#include "hkscaux.h" /* auxiliary functions for standalone program */


#define HKSC_NAME "Hksc" /* default program name */

#define HKSC_VERSION "0.0"

static int listing=0;     /* list bytecodes? */
static int dumping=1;     /* dump bytecodes? */

static int mode=HKSC_MODE_DEFAULT; /* compiling or decompiling? */

/* parser settings */
static int striplevel=BYTECODE_STRIPPING_NONE; /* bytecode stripping level */
static int literals_enabled=INT_LITERALS_NONE; /* int literal options */
static const char *progname=HKSC_NAME;
const char *output=NULL;

#ifdef LUA_COD
const char *debugfile=NULL;
const char *callstackdb=NULL;
static int withdebug=0;
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
   "Available options are:\n"
   "  -c, --compile         Run Hksc in compile mode\n"
   "  --printconfig         Print Hksc configuration\n"
   "  -d, --decompile       Run Hksc in decompile mode\n"
#ifdef LUA_COD
   "  --withdebug           Load/dump debug files with input files\n"
   "  --callstackdb <name>  Use callstack reconstruction file <name>\n"
   "  --debugfile <name>    Use debug info file <name>\n"
#else
   "  --ignoredebug         Ignore debug info when decompiling\n"
#endif /* LUA_COD */
   , stderr);
  fputs(
   "  -h, --help            Print this message and exit\n"
   "  -l                    List (use -l -l for full listing)\n"
   "  -L <type>             Enable int literals of the type given by <type>\n"
   "  -o, --output <name>   Output to file 'name'\n"
   "  -p                    Parse only\n"
#ifndef LUA_COD /* -s is still enabled for testing purposes */
   "  -s <mode>             Use bytecode stripping mode <mode>\n"
#endif /* !LUA_COD */
   "  -v, --version         Show version information\n"
   "  --                    Stop handling options\n", stderr);
  fputs(
   "\nInt literal options (to use with '-L')\n"
   "  I or 32  [32BIT] Enable 32-bit int literals\n"
   "  L or 64  [64BIT] Enable 64-bit int literals\n"
   "  A        [ALL] Enable all int literals\n"
#ifndef LUA_COD /* use special arguments for cod */
   "\nBytecode stripping levels (to use with '-s'):\n"
   "  N or 0   [NONE] Include all debug information in dump\n"
   "  P or 1   [PROFILING] Include line information in dump\n"
   "  A or 2   [ALL] Strip all debug information\n"
#endif /* !LUA_COD */
   , stderr);
}

static void usage(const char *message)
{
  if (*message == '-')
    fprintf(stderr,"%s: unrecognized option '%s'\n",progname,message);
  else
    fprintf(stderr,"%s: %s\n",progname,message);
  print_usage();
  exit(EXIT_FAILURE);
}

static void print_version(void)
{
  printf(HKSC_NAME " " HKSC_VERSION "\n" /* Hksc version */
#ifdef LUA_COD
    "Call of Duty Lua compiler/decompiler\n"
#endif
    LUA_VERSION " " LUA_COPYRIGHT "\n" /* Lua version */
  );
}

#define HKSC_YESNO(x) ((HKSC_##x) ? "YES" : "NO")

static void print_config(void)
{
  print_version();
  fputs("\nHksc configuration:\n", stdout);
  fprintf(stdout, "  GETGLOBAL_MEMOIZATION     %s\n",
          HKSC_YESNO(GETGLOBAL_MEMOIZATION));
  fprintf(stdout, "  STRUCTURE_EXTENSION_ON    %s\n",
          HKSC_YESNO(STRUCTURE_EXTENSION_ON));
  fprintf(stdout, "  SELF                      %s\n",
          HKSC_YESNO(SELF));
  fprintf(stdout, "  WITHDOUBLES               %s\n",
          HKSC_YESNO(WITHDOUBLES));
  fprintf(stdout, "  WITHNATIVEINT             %s\n",
          HKSC_YESNO(WITHNATIVEINT));
}

#define IS(s) (strcmp(argv[i],s)==0)
#define HAS(s) (strncmp(argv[i],"" s,sizeof(s)-1)==0)
#define CHECKARGC if (i >= argc) usage("argument expected")

#define DOSTRINGARG(str, var) do { \
  char *val = argv[i] + sizeof(str)-1; \
  if (*val == '=') val++; \
  else if (*val == '\0') val = argv[++i]; \
  else usage(argv[i]); \
  if (val == NULL || *val == 0) \
    usage("argument exptected with '" str "'"); \
  var = (const char *)val; \
} while (0)

#define ELSE_IF_STRING(str, var) else if (HAS(str)) DOSTRINGARG(str, var)

static int doargs(int argc, char *argv[])
{
  int i;
  int striparg=0;
  int version=0;
  int info=0;
  int c=0,d=0; /* uses of `-c' and `-d' */
  if (argv[0]!=NULL && *argv[0]!=0) progname=argv[0];
  for (i=1; i<argc; i++)
  {
    if (*argv[i]!='-')      /* end of options; keep it */
      break;
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
    else if (IS("-c") || IS("--compile")) ++c; /* specifies compile mode */
#ifdef LUA_COD
    else if (IS("--withdebug")) withdebug=1;
    ELSE_IF_STRING("--callstackdb", callstackdb);
    ELSE_IF_STRING("--debugfile", debugfile);
#else
    else if (IS("--ignoredebug")) ignore_debug=1;
#endif /* LUA_COD */
    else if (IS("--printconfig")) ++info;
    else if (IS("-d") || IS("--decompile")) ++d; /* specified decompile mode */
    else if (IS("-h") || IS("--help")) { /* print help message and exit */
      print_usage();
      exit(EXIT_SUCCESS);
    }
    else if (IS("-l"))      /* list */
      ++listing;
    else if (HAS("-L"))     /* specify int literal options */
    {
      char *mode;
      if (argv[i][2] == 0) /* next argument has int literal type */
        mode = argv[++i];
      else
        mode = argv[i]+2;
      CHECKARGC;
      if (mode[0] == 0 || (mode[1] != 0 && mode[2] != 0))
        goto badliteralarg;
      switch (*mode) {
        case 'I': case 'i': case '3':
          if ((mode[0] != '3' && mode[1] != '\0') ||
              (mode[0] == '3' && mode[1] != '2')) goto badliteralarg;
          literals_enabled|=INT_LITERALS_LUD; break;
        case 'L': case 'l': case '6':
          if ((mode[0] != '6' && mode[1] != '\0') ||
              (mode[0] == '6' && mode[1] != '4')) goto badliteralarg;
          literals_enabled|=INT_LITERALS_UI64; break;
        case 'A': case 'a':
          if (mode[1] != 0) goto badliteralarg;
          literals_enabled|=INT_LITERALS_ALL; break;
        default:
          goto badliteralarg;
      }
      continue;
      badliteralarg:
      usage("invalid int literal type given with '-L'");
    }
    ELSE_IF_STRING("--output", output);
    else if (IS("-o"))      /* output file */
    {
      output=argv[++i];
      if (output==NULL || *output==0 || (*output=='-' && output[1]!=0))
        usage("'-o' needs argument");
      if (IS("-")) output=NULL;
    }
    else if (IS("-p"))      /* parse only */
      dumping=0;
    else if (HAS("-s"))     /* specify stripping level */
    {
      char *mode;
      if (striparg)
        usage("'-s' option used multiple times");
      if (argv[i][2] == 0) /* next agrument specifies stripping mode */
        mode = argv[++i];
      else
        mode = argv[i]+2;
      CHECKARGC;
      if (mode[0] == 0 || (mode[1] != 0 && mode[1] != ','))
        goto badstriparg;
      switch (*mode) {
        case 'N': case 'n': case '0':
          striplevel=BYTECODE_STRIPPING_NONE; break;
        case 'P': case 'p': case '1':
          striplevel=BYTECODE_STRIPPING_PROFILING; break;
        case 'A': case 'a': case '2':
          striplevel=BYTECODE_STRIPPING_ALL; break;
#if 0
        case 'D': case 'd': case '3':
          striplevel=BYTECODE_STRIPPING_DEBUG_ONLY; break;
        case 'C': case 'c': case '4':
          striplevel=BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION; break;
#endif
        default:
          goto badstriparg;
      }
      striparg=1;
      continue;
      badstriparg:
      usage("invalid stripping mode given with '-s'");
    }
    else if (IS("-v") || IS("--version"))     /* show version */
      ++version;
    else
      usage(argv[i]);
  }
  if (version || info)
  {
    if (!info) print_version();
    else print_config();
    if (i==argc) exit(EXIT_SUCCESS);
  }
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
  if (!dumping && striparg)
    warn_unused("-s", "not dumping");
  return i;
}

#define FUNCTION "(function()end)();"

static hksc_DumpFunction dumpf;

/* dump function for -l */
static int hksc_dump_l(hksc_State *H, const Proto *f, void *ud) {
  (void)H; (void)ud;
  luaU_print(f, listing > 1);
  return 0;
}

/* dump function for -p */
static int hksc_dump_p(hksc_State *H, const Proto *f, void *ud) {
  (void)H; (void)f;
  fprintf(stderr, "Successfully parsed `%s'\n", (const char *)ud);
  return 0;
}

/* default dump function */
static int hksc_dump_default(hksc_State *H, const Proto *f, void *ud) {
  return hksc_dump_function(H, f, (const char *)ud);
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
        fprintf(stderr, "%s\n", luaE_geterrormsg(H));
        luaE_clearerr(H); /* discharge the error message */
      } else {
        fprintf(stderr, "%s: %s\n", progname, luaE_geterrormsg(H));
        break; /* fatal */
      }
    }
  }
  return error;
}


int main(int argc, char *argv[])
{
  hksc_State *H;
  int status;
  int i=doargs(argc,argv);
  argc-=i;argv+=i;
  if (argc<=0) usage("no input files given");
  /* warn about using -o with multiple input files */
  else if (argc > 1) {
    if (output != NULL)
      error_multiple_inputs("-o");
#ifdef LUA_COD
    if (debugfile != NULL)
      error_multiple_inputs("--debugfile");
    if (callstackdb != NULL)
      error_multiple_inputs("--callstackdb");
#endif /* LUA_COD */
  }
  H = hksI_newstate(mode);
  if (H==NULL) fatal("cannot create state: not enough memory");
#ifdef LUA_COD
  if (dumping) {
    hksc_onstartcycle(H, luacod_startcycle);
    hksc_onendcycle(H, luacod_endcycle);
  }
#endif /* LUA_COD */
  hksc_setIntLiteralsEnabled(H,literals_enabled);
#ifdef LUA_COD
  Settings(H).ignore_debug=!withdebug;
  hksc_setBytecodeStrippingLevel(H,BYTECODE_STRIPPING_ALL);
# ifdef HKSC_DECOMPILER
  /* Call of Duty needs a separate debug reader when loading bytecode */
  G(H)->debugLoadStateOpen = init_debug_reader;
  G(H)->debugLoadStateClose = close_debug_reader;
# endif /* HKSC_DECOMPILER */
#else /* !LUA_COD */
  hksc_setBytecodeStrippingLevel(H,striplevel);
  Settings(H).ignore_debug=ignore_debug;
#endif /* LUA_COD */
  if (listing)
    dumpf = hksc_dump_l;
  else if (!dumping)
    dumpf = hksc_dump_p;
  else
    dumpf = hksc_dump_default;
  status = dofiles(H, argc, argv);
  printf("hksc: closing hksc_State\n");
  hksI_close(H);
  printf("hksc: closed hksc_State\nExiting with code %d\n",
         status ? EXIT_FAILURE : EXIT_SUCCESS);
  return status ? EXIT_FAILURE : EXIT_SUCCESS;
}
