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


static int listing=0;     /* list bytecodes? */
static int dumping=1;     /* dump bytecodes? */
#ifdef HKSC_DECOMPILER
static int decompiling=0;
#endif /* HKSC_DECOMPILER */

static int mode=HKSC_MODE_DEFAULT; /* compiling or decompiling? */

/* parser settings */
static int striplevel=BYTECODE_STRIPPING_NONE; /* bytecode stripping level */
static int literals_enabled=INT_LITERALS_NONE; /* int literal options */
static const char *progname=HKSC_NAME;
const char *output=NULL;

static int withdebug=0;

#ifdef HKSC_LOGGING
static const char *logfilename=NULL;
static FILE *logfile=NULL;
#endif /* HKSC_LOGGING */

#ifdef LUA_COD
const char *debugfile=NULL;
const char *callstackdb=NULL;
int debugfile_arg=0;
int callstackdb_arg=0;
#else
static int ignore_debug=0;
#endif /* LUA_COD */

static const char *file_prefix_map_arg=NULL;

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
   "      --print-config      Print Hksc configuration\n"
   "  -a, --source            Expect source files as input\n"
   "  -b, --binary            Expect binary files as input\n"
  , stderr);
  fputs(
   "\nCompiler options:\n"
   "  -L[=TYPE]               Enable int literals of the given TYPE\n"
#ifndef LUA_COD
   "  -s[=MODE]               Use bytecode stripping level MODE\n"
   "  -i, --ignoredebug       Ignore debug info when decompiling\n"
#else
   "  -s                      Turns off --withdebug even if set explicitly\n"
#endif /* LUA_COD */
   , stderr);
  fputs(
   "\nInput/Output options:\n"
   "  -o, --output=NAME       Output to file NAME\n"
#ifdef HKSC_DECOMPILER
   "  -d                      Decompile\n"
#endif
   "  -l                      List (use -l -l for full listing)\n"
   "  -p                      Parse only\n"
#ifndef LUA_COD
   "  -g, --withdebug         Load/dump debug information\n"
#else
   "  -g, --withdebug         Load/dump debug files with input/output files\n"
   "  -a, --callstackdb=FILE  Use FILE for callstack reconstruction\n"
   "  -g, --debugfile=FILE    Use FILE for debug info\n"
#endif /* LUA_COD */
#ifdef HKSC_LOGGING
   "      --logfile=FILE      Output logs to FILE\n"
#endif /* HKSC_LOGGING */
   , stderr);
  fputs(
   "\nOther Options:\n"
   "      --file-prefix-map=<OLD=NEW>\n"
   "                          Remap file source paths in debug info\n"
   "      --                  Stop handling options\n", stderr);
  fputs(
   "\nInt literal options for TYPE (to use with `-L')\n"
   "  32  Enable 32-bit int literals\n"
   "  64  Enable 64-bit int literals\n"
   "  Not providing a value for TYPE will enable all literal types\n",
   stderr);
#ifndef LUA_COD /* use special arguments for cod */
  fputs(
   "\nBytecode stripping options for MODE (to use with `-s'):\n"
   "  n   Include all debug information in dump\n"
   "  p   Include profiling information in dump\n"
   "  a   Ignore all debug information in dump\n"
   "  Not providing a value for MODE is equivalent to providing `a'\n"
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
  fputs(HKSC_NAME " " HKSC_VERSION "\n" /* Hksc version */
  LUA_VERSION " " LUA_COPYRIGHT "\n" /* Lua version */
  , stdout);
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
  fputc('\n', stdout);
  fputs("Library features:\n", stdout);
#ifdef HKSC_DECOMPILER
  fputs("  Decompiler             Enabled\n", stdout);
#else /* !HKSC_DECOMPILER */
  fputs("  Decompiler             Disabled\n", stdout);
#endif /* HKSC_DECOMPILER */
#ifdef HKSC_LOGGING
  fputs("  Logging                Enabled\n", stdout);
#else /* !HKSC_LOGGING */
  fputs("  Logging                Disabled\n", stdout);
#endif /* HKSC_LOGGING */
  fputc('\n', stdout);
  fputs("Call of Duty settings:\n", stdout);
#ifdef LUA_COD
  fputs("  T6 extensions          Enabled\n", stdout);
#else /* !LUA_COD */
  fputs("  T6 extensions          Disabled\n", stdout);
#endif /* LUA_COD */
#ifdef LUA_CODT7
  fputs("  T7 extensions          Enabled\n", stdout);
#else /* !LUA_CODT7 */
  fputs("  T7 extensions          Disabled\n", stdout);
#endif /* LUA_CODT7 */
}

#define IS(s) (strcmp(argv[i],s)==0)
#define HAS(s) (strncmp(argv[i],"" s,sizeof(s)-1)==0)

#define DOSTRINGARG(s,l,v) do { \
  char *val; \
  int shrt; \
  if (s != NULL && (shrt = IS(s))) val = argv[++i]; \
  else { \
    val = argv[i] + sizeof(l)-1; \
    if (*val == '=') val++; \
    else if (*val == '\0') val = argv[++i]; \
    else usage(argv[i]); \
  } \
  if (val == NULL || *val == '\0') { \
    if (s != NULL && shrt) usage("'"  "' needs an argument"); \
    else usage("'" l "' needs an argument"); \
  } \
  v = (const char *)val; \
} while (0)

#define ELSE_IF_STRING(s,l,v) \
  else if (((s != NULL) && IS(s)) || HAS(l)) DOSTRINGARG(s,l,v)

static int doargs(int argc, char *argv[])
{
  int i;
  int nfiles=0;
  int striparg=0;
  int version=0;
  int info=0;
  int a=0,b=0; /* uses of `-a' and `-b' */
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
    else if (IS("-a") || IS("--source")) ++a; /* specifies compile mode */
    else if (IS("-b") || IS("--binary")) ++b; /* specified decompile mode */
    else if (IS("-r") || IS("--withdebug")) withdebug=1;
#ifdef LUA_COD
    ELSE_IF_STRING("-a", "--callstackdb", callstackdb);
    ELSE_IF_STRING("-g", "--debugfile", debugfile);
#else
    else if (IS("-i") || IS("--ignoredebug")) ignore_debug=1;
#endif /* LUA_COD */
    else if (IS("--print-config")) ++info;
    else if (IS("--help")) { /* print help message and exit */
      print_usage();
      exit(EXIT_SUCCESS);
    }
    else if (IS("-l"))      /* list */
      ++listing;
#ifdef HKSC_DECOMPILER
    else if (IS("-d") || IS("--decompile")) {
      decompiling=1;
      dumping=1;
    }
#endif /* HKSC_DECOMPILER */
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
    ELSE_IF_STRING("-v", "--file-prefix-map", file_prefix_map_arg);
    ELSE_IF_STRING("-o", "--output", output);
#ifdef HKSC_LOGGING
    ELSE_IF_STRING(NULL, "--logfile", logfilename);
#endif /* HKSC_LOGGING */
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
  if (a && b) /* both compile and decompile mode specified? */
    usage("both '-a' and '-b' used; Hksc can only be run in one mode");
  else if (a) {
#ifndef LUA_COD
    if (ignore_debug)
      warn_unused("--ignoredebug", "compiling");
#endif /* !LUA_COD */
    mode=HKSC_MODE_SOURCE;
  }
  else if (b)
    mode=HKSC_MODE_BINARY;
  if (striparg && (dumping))
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
  const char *filename = (const char *)ud;
#ifdef HKSC_DECOMPILER
  if (decompiling)
    return hksc_dump_decomp(H, filename);
#endif /* HKSC_DECOMPILER */
  return hksc_dump_bytecode(H, filename);
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
  const char *old_prefix, *new_prefix;
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
  hksI_StateSettings(&settings);
#ifdef HKSC_LOGGING
  if (logfilename != NULL && *logfilename != '\0') {
    logfile = fopen(logfilename, "w");
    if (!logfile) {
      fatal("cannot open log file");
    }
    settings.logctx.ud = logfile;
  }
#endif /* HKSC_LOGGING */
  if (file_prefix_map_arg) {
    old_prefix = file_prefix_map_arg;
    new_prefix = strrchr(file_prefix_map_arg, '=');
    if (!new_prefix)
      usage("invalid value for --file-prefix-map");
    *((char *)new_prefix) = '\0';
    new_prefix++;
  }
  else
    new_prefix = old_prefix = NULL;
  H = hksI_newstate(&settings);
  if (H==NULL) fatal("cannot create state: not enough memory");
  lua_setprefixmap(H, old_prefix, new_prefix);
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
#ifdef HKSC_LOGGING
  if (logfile) fclose(logfile);
#endif /* HKSC_LOGGING */
  return status ? EXIT_FAILURE : EXIT_SUCCESS;
}
