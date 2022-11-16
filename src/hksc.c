/*
** $Id: hksc.c,v 1.74 2015/03/12 01:53:53 lhf Exp lhf $
** Lua compiler (saves bytecodes to files; also lists bytecodes)
** See Copyright Notice in lua.h
*/

#define hksc_c
/* #define LUA_CORE */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "luaconf.h"

#include "hksclib.h"

#include "lctype.h"
#include "lundump.h"


#define HKSC_NAME "Hksc" /* default program name */

#define HKSC_VERSION "0.0"

static int listing=0;     /* list bytecodes? */
static int dumping=1;     /* dump bytecodes? */

static int mode=HKSC_MODE_DEFAULT; /* compiling or decompiling? */

/* parser settings */
static int striplevel=BYTECODE_STRIPPING_NONE; /* bytecode stripping level */
static int literals_enabled=INT_LITERALS_NONE; /* int literal options */
static const char *progname=HKSC_NAME;
static const char *output=NULL;

static void fatal(const char *message)
{
  fprintf(stderr,"%s: %s\n",progname,message);
  exit(EXIT_FAILURE);
}

static void print_usage(void)
{
  fprintf(stderr,
   "usage: %s [options] [filenames]\n"
   "Available options are:\n"
   "  -c       Run Hksc in compile mode\n"
   "  --config Print Hksc configuration\n"
   "  -d       Run Hksc in decompile mode\n"
   "  -h       Print this message and exit\n"
   "  -l       List (use -l -l for full listing)\n"
   "  -L type  Enable int literals of the type given by <type>\n"
   "  -o name  Output to file 'name'\n"
   "  -p       Parse only\n"
   "  -s mode  Use bytecode stripping mode <mode>\n"
   "  -v       Show version information\n"
   "  --       Stop handling options\n", progname);
  fputs(
   "\nInt literal options (to use with '-L')\n"
   "  I or 32  [32BIT] Enable 32-bit int literals\n"
   "  L or 64  [64BIT] Enable 64-bit int literals\n"
   "  A        [ALL] Enable all int literals\n"
   "\nBytecode stripping levels (to use with '-s'):\n"
   "  N or 0   [NONE] Include all debug information in dump\n"
   "  P or 1   [PROFILING] \n"
   "  A or 2   [ALL] Strip all debug information\n"
#ifdef LUA_COD
   "  D or 3   [DEBUG_ONLY] Only dump debug information\n"
   "  C or 4   [CALLSTACK_RECONSTRUCTION] \n"
#endif /* LUA_COD */
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
    LUA_VERSION " " LUA_COPYRIGHT "\n" /* Lua version */
  );
}

#define HKSC_YESNO(x) ((HKSC_##x) ? "YES" : "NO")

static void print_config(void)
{
  print_version();
  fputs("\nHksc configuration:\n", stdout);
  fprintf(stdout, "  WITH GLOBAL MEMOIZATION   %s\n",
          HKSC_YESNO(GETGLOBAL_MEMOIZATION));
  fprintf(stdout, "  WITH STRUCTURES           %s\n",
          HKSC_YESNO(STRUCTURE_EXTENSION_ON));
  fprintf(stdout, "  WITH SELF                 %s\n", HKSC_YESNO(SELF));
  fprintf(stdout, "  WITH DOUBLES              %s\n", HKSC_YESNO(WITHDOUBLES));
  fprintf(stdout, "  WITH NATIVE INT           %s\n",
          HKSC_YESNO(WITHNATIVEINT));
}

#define IS(s) (strcmp(argv[i],s)==0)
#define HAS(s) (strncmp(argv[i],"" s,sizeof(s)-1)==0)
#define CHECKARGC if (i >= argc) usage("argument expected")

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
    else if (IS("-c")) ++c; /* specifies compile mode */
    else if (IS("--config")) ++info;
    else if (IS("-d")) ++d; /* specified decompile mode */
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
          if (mode[0] == '3' && mode[1] != '2') goto badliteralarg;
          literals_enabled=INT_LITERALS_LUD; break;
        case 'L': case 'l': case '6':
          if (mode[0] == '6' && mode[1] != '4') goto badliteralarg;
          literals_enabled=INT_LITERALS_UI64; break;
        case 'A': case 'a':
          if (mode[1] != 0) goto badliteralarg;
          literals_enabled=INT_LITERALS_ALL; break;
        default:
          goto badliteralarg;
      }
      continue;
      badliteralarg:
      usage("invalid int literal type given with '-L'");
    }
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
      if (argv[i][2] == 0) /* next agrument specifies stripping mode */
        mode = argv[++i];
      else
        mode = argv[i]+2;
      CHECKARGC;
      if (mode[0] == 0 || mode[1] != 0)
        goto badstriparg;
      switch (*mode) {
        case 'N': case 'n': case '0':
          striplevel=BYTECODE_STRIPPING_NONE; break;
        case 'P': case 'p': case '1':
          striplevel=BYTECODE_STRIPPING_PROFILING; break;
        case 'A': case 'a': case '2':
          striplevel=BYTECODE_STRIPPING_ALL; break;
        case 'D': case 'd': case '3':
          striplevel=BYTECODE_STRIPPING_DEBUG_ONLY; break;
        case 'C': case 'c': case '4':
          striplevel=BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION; break;
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
    usage("both '-c' and '-d' given; Hksc can only be run in one mode");
  else if (c)
    mode=HKSC_MODE_COMPILE;
  else if (d)
    mode=HKSC_MODE_DECOMPILE;
  if (!dumping && striparg)
    fprintf(stderr, "warning: '-s' argument is ignored when not dumping "
      "('-p')\n");
  return i;
}

#define FUNCTION "(function()end)();"


/*
** parser loop functions
*/
#define DECL_PARSER_LOOP_FUNC(name, expr) \
static int name (hksc_State *H, int argc, char *argv[]) { \
  int i,status,error=0; \
  for (i=0; i<argc; i++) { \
    error |= (status = (expr)); \
    if (status) { \
      if (status == LUA_ERRSYNTAX){ \
        fprintf(stderr, "%s\n", luaE_geterrormsg(H)); \
        luaE_clearerr(H); /* discharge the error message */ \
      } \
      else { \
        fprintf(stderr, "%s: %s\n", progname, luaE_geterrormsg(H)); \
        break; /* non-syntax errors are fatal */ \
      } \
    } \
  } \
  return error; \
}

/* parse but do not dump */
DECL_PARSER_LOOP_FUNC(parseonly, hksI_parser_file(H, argv[i]))
/* parse and dump (output is NULL if multiple input files) */
DECL_PARSER_LOOP_FUNC(parseanddump, hksI_parser_file2file(H, argv[i], output))


int main(int argc, char *argv[])
{
  hksc_State *H;
  int status;
  int i=doargs(argc,argv);
  argc-=i;argv+=i;
  if (argc<=0) usage("no input files given");
  /* warn about using -o with multiple input files */
  else if (argc > 1 && output != NULL)
    usage("'-o' option used with multiple input files");
  H = hksI_newstate(mode);
  if (H==NULL) fatal("cannot create state: not enough memory");
  hksc_setBytecodeStrippingLevel(H,striplevel);
  hksc_setIntLiteralsEnabled(H,literals_enabled);
  if (!dumping)
    status=parseonly(H,argc,argv);
  else
    status=parseanddump(H,argc,argv);
  printf("hksc: closing hksc_State\n");
  hksI_close(H);
  printf("hksc: closed hksc_State\nExiting with code %d\n",
         status ? EXIT_FAILURE : EXIT_SUCCESS);
  return status ? EXIT_FAILURE : EXIT_SUCCESS;
}
