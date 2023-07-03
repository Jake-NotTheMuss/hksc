/*
** $Id: hksc.c $
** Lua compiler (saves bytecodes to files; also lists bytecodes)
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

#define hksc_c

#include "hksclua.h"

#include "hksclib.h"

#include "hkscaux.h" /* auxiliary functions for standalone program */


#define HKSC_NAME "hksc" /* default program name */

static int listing=0;     /* list bytecodes? */
static int dumping=1;     /* dump bytecodes? */
static int compiling=0;
#ifdef HKSC_DECOMPILER
static int decompiling=0;
#endif /* HKSC_DECOMPILER */

static int mode=HKSC_MODE_DEFAULT; /* compiling or decompiling? */

/* parser settings */
static int striplevel=BYTECODE_STRIPPING_NONE; /* bytecode stripping level */
static int literals_enabled=INT_LITERALS_NONE; /* int literal options */
static const char *progname=HKSC_NAME;
const char *output=NULL;

#ifdef LUA_CODT6
int withdebug=0;
#endif /* LUA_CODT6 */

#ifdef HKSC_MULTIPLAT
static int target_plat=HKSC_TARGET_PLAT_DEFAULT;
static int target_ws=HKSC_TARGET_WS_DEFAULT;
#endif /* HKSC_MULTIPLAT */

#ifdef LUA_CODT6
const char *debugfile=NULL;
const char *profilefile=NULL;
int debugfile_arg=0;
int profilefile_arg=0;
#endif /* LUA_CODT6 */

static int ignore_debug=0;

#define STRINGIFYARG(x) #x
#define STRINGIFY(x) STRINGIFYARG(x)

#define MAX_PREFIX_MAPS 32
static int nprefixmaps = 0;
static const char *file_prefix_maps[MAX_PREFIX_MAPS];

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
   "      --version           Show version information and exit\n"
   "      --print-config      Show build configuration and exit\n"
   "  -a, --source            Expect source files as input\n"
   "  -b, --binary            Expect binary files as input\n"
   "  -c, --compile           Compile input files\n"
#ifdef HKSC_DECOMPILER
   "  -d, --decompile         Decompile input files\n"
#endif
   "  -l, --list              List (use -l -l for full listing)\n"
   "  -p, --parse             Parse only\n"
  , stderr);
  fputs(
   "\nCompiler options:\n"
   "  -L[=TYPE]               Enable int literals of the given TYPE\n"
#ifndef LUA_CODT6
   "  -s[=MODE]               Use bytecode stripping level MODE\n"
#else
   "  -s                      Do not dump debug information\n"
   "  -g, --with-debug        Load/dump debug information with input/output "
   "files\n"
#endif
   "  -i, --ignore-debug      Ignore debug info when loading bytecode\n"
   , stderr);
  fputs(
   "\nInput/Output options:\n"
   "  -o, --output=FILE       Output to file FILE\n"
#ifdef LUA_CODT6
   "      --callstackdb=FILE  Dump callstack reconstruction to FILE\n"
   "      --debugfile=FILE    Use FILE for loading/dumping debug information\n"
#endif
   , stderr);
  fputs(
   "\nOther Options:\n"
   "      --file-prefix-map=<OLD=NEW>\n"
   "                          Remap file source paths in debug info\n"
#ifdef HKSC_MULTIPLAT
   "  -m<PLATFORM>            Load/dump bytecode for the given PLATFORM\n"
   "  -m<16|32|64>            Load/dump bytecode with given word size\n"
#endif
   "      --                  Stop handling options\n", stderr);
  fputs(
   "\nInt literal options for TYPE (to use with '-L')\n"
   "  32  Enable 32-bit int literals\n"
   "  64  Enable 64-bit int literals\n"
   "  Not providing a value for TYPE will enable all literal types\n",
   stderr);
#ifndef LUA_CODT6 /* use special arguments for cod */
  fputs(
   "\nBytecode stripping options for MODE (to use with '-s')\n"
   "  n   Include all debug information in dump\n"
   "  p   Include profiling information in dump\n"
   "  a   Ignore all debug information in dump\n"
   "  Not providing a value for MODE is equivalent to providing 'a'\n"
   , stderr);
#endif /* !LUA_CODT6 */
#ifdef HKSC_MULTIPLAT
  fputs(
   "\nPLATFORM names (to use with '-m')\n"
   /* most of these platforms are the same where it matters to the compiler,
   i.e. when it comes to word-size and integer-size - providing options for
   all of these platforms is to avoid confusion for the user when, for example,
   they want to target macos, where `-mwindows' would suffice, but they probably
   want to do `-mdarwin' */
   "  wii\n"
   "  wiiu\n"
   "  nx\n"
   "  ps3\n"
   "  orbis\n"
   "  xenon\n"
   "  durango\n"
   "  windows\n"
   "  gnu\n"
   "  darwin\n"
   "'-m' options to target the default platform with specified word-sizes\n"
   "  16   (16-bit integers, 16-bit addresses)\n"
   "  32   (32-bit integers, 32-bit addresses)\n"
   "  64   (32-bit integers, 64-bit addresses)\n"
   , stderr);
#endif /* HKSC_MULTIPLAT */
}


static void usage(const char *fmt, ...)
{
  if (*fmt == '-') {
    fprintf(stderr,"%s: unrecognized option '%s'\n",progname,fmt);
    fprintf(stderr,"Try '%s --help' for more information.\n",progname);
  }
  else {
    va_list argp;
    fprintf(stderr,"%s: ",progname);
    va_start(argp, fmt);
    vfprintf(stderr,fmt,argp);
    va_end(argp);
    fputc('\n',stderr);
  }
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
  fputs("  Decompiler                 Enabled\n", stdout);
#else /* !HKSC_DECOMPILER */
  fputs("  Decompiler                 Disabled\n", stdout);
#endif /* HKSC_DECOMPILER */
#ifdef HKSC_MULTIPLAT
  fputs("  Multi-platform targeting   Enabled\n", stdout);
#else /* !HKSC_MULTIPLAT */
  fputs("  Multi-platform targeting   Disabled\n", stdout);
#endif /* HKSC_MULTIPLAT */
  fputc('\n', stdout);
  fputs("Bytecode compatibility settings:\n", stdout);
#ifdef LUA_CODT6
  fputs("  T6 extensions              Enabled\n", stdout);
#else /* !LUA_CODT6 */
  fputs("  T6 extensions              Disabled\n", stdout);
#endif /* LUA_CODT6 */
#ifdef LUA_CODT7
  fputs("  T7 extensions              Enabled\n", stdout);
#else /* !LUA_CODT7 */
  fputs("  T7 extensions              Disabled\n", stdout);
#endif /* LUA_CODT7 */
#ifdef LUA_CODIW6
  fputs("  IW6 extensions             Enabled\n", stdout);
#else /* !LUA_CODIW6 */
  fputs("  IW6 extensions             Disabled\n", stdout);
#endif /* LUA_CODIW6 */
#ifdef HKSC_FROMSOFT_TTABLES
  fputs("  FromSoftware hashtables    Enabled\n", stdout);
#else /* !LUA_CODIW6 */
  fputs("  FromSoftware hashtables    Disabled\n", stdout);
#endif /* LUA_CODIW6 */
}

#ifdef HKSC_MULTIPLAT
static int hksc_casecmp(const char *str1, const char *str2)
{
  for (; *str1 && *str2; str1++, str2++) {
    if (tolower(*str1) != tolower(*str2))
      return 0;
  }
  return (*str1 == *str2);
}

static int hksc_casencmp(const char *str1, const char *str2, size_t n)
{
  size_t i = 0;
  for (; *str1 && *str2; str1++, str2++) {
    if (i++ >= n)
      return 1;
    if (tolower(*str1) != tolower(*str2))
      return 0;
  }
  return (i >= n) || (*str1 == *str2);
}
#endif /* HKSC_MULTIPLAT */

#define STREQ(a,b) (hksc_casecmp(a,b))
#define STREQN(a,b) (hksc_casencmp(a,"" b,sizeof(b)-1))
#define IS(s) (strcmp(argv[i],s)==0)
#define HAS(s) (strncmp(argv[i],"" s,sizeof(s)-1)==0)

#define DO_ARG(opt,v) do { \
  char *val = argv[i] + sizeof(opt)-1; \
  if (*val == '=') val++; \
  else if (*val == '\0') { \
    if (++i >= argc) usage("'" opt "' needs an argument"); \
    val = argv[i]; \
  } \
  else usage(argv[i]); \
  if (val == NULL || *val == '\0') \
    usage("'" opt "' needs an argument"); \
  v = (const char *)val; \
} while (0)

#define CHECK_OPT(s,l,v) \
  else if (HAS(s)) DO_ARG(s,v); \
  else if (HAS(l)) DO_ARG(l,v)

#define CHECK_LONG_OPT(l,v)  else if (HAS(l)) DO_ARG(l,v)

#define CHECK_SHORT_OPT(s,v)  else if (HAS(s)) DO_ARG(s,v)

static int doargs(int argc, char *argv[])
{
  int i;
  int nfiles=0;
  int striparg=0;
  int version=0;
  int info=0;
  int a=0,b=0; /* uses of `-a' and `-b' */
  int explicit_dumping=0;
  const char *opt_a=NULL, *opt_b=NULL;
#ifdef LUA_CODT6
  const char *opt_withdebug=NULL;
#endif /* LUA_CODT6 */
#ifdef HKSC_DECOMPILER
  const char *opt_compile=NULL, *opt_decompile=NULL;
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
    else if (IS("-a") || IS("--source")) {
      ++a;
      opt_a = (const char *)argv[i];
    }
    else if (IS("-b") || IS("--binary")) {
      ++b;
      opt_b = (const char *)argv[i];
    }
#ifdef LUA_CODT6
    else if (IS("-g") || IS("--with-debug")) {
      withdebug=1;
      opt_withdebug = (const char *)argv[i];
    }
    CHECK_LONG_OPT("--callstackdb", profilefile);
    CHECK_LONG_OPT("--debugfile", debugfile);
#endif /* LUA_CODT6 */
    else if (IS("-i") || IS("--ignore-debug")) ignore_debug=1;
    else if (IS("--print-config")) ++info;
    else if (IS("--help")) { /* print help message and exit */
      print_usage();
      exit(EXIT_SUCCESS);
    }
    else if (IS("-l") || IS("--list")) {     /* list */
      ++listing;
      if (!explicit_dumping)
        dumping=0;
    }
    else if (IS("-c") || IS("--compile")) {
      compiling=1;
      dumping=1;
      explicit_dumping=1;
#ifdef HKSC_DECOMPILER
      opt_compile = (const char *)argv[i];
#endif /* HKSC_DECOMPILER */
    }
#ifdef HKSC_DECOMPILER
    else if (IS("-d") || IS("--decompile")) {
      decompiling=1;
      dumping=1;
      explicit_dumping=1;
      opt_decompile = (const char *)argv[i];
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
    else if (HAS("--file-prefix-map"))
    {
      const char *prefixmaparg;
      DO_ARG("--file-prefix-map", prefixmaparg);
      if (strchr(prefixmaparg, '=') == NULL)
        usage("invalid use of '--file-prefix-map'");
      if (nprefixmaps < MAX_PREFIX_MAPS)
        file_prefix_maps[nprefixmaps++] = prefixmaparg;
      else
        fatal("too many file prefix maps (maximum of "
              STRINGIFY(MAX_PREFIX_MAPS) " allowed)");
    }
    CHECK_OPT("-o", "--output", output);
#ifdef HKSC_MULTIPLAT
    else if (HAS("-m"))
    {
      int optionalnumber = 0; /* numeric suffix is optional */
      char *arg;
      if (argv[i][2] == 0) {
        if (++i >= argc) usage("'-m' needs an argument");
        arg = argv[i];
      }
      else if (argv[i][2] == '=')
        arg = &argv[i][3];
      else
        arg = &argv[i][2];
      if (*arg == '\0')
        usage("'-m' needs an argument");
      else if (STREQ(arg, "wii"))
        target_plat = HKSC_TARGET_PLAT_WII;
      else if (STREQ(arg, "wiiu") || STREQ(arg, "cafe"))
        target_plat = HKSC_TARGET_PLAT_CAFE;
      else if (STREQ(arg, "switch") || STREQ(arg, "nx"))
        target_plat = HKSC_TARGET_PLAT_NX;
      else if (STREQ(arg, "ps3"))
        target_plat = HKSC_TARGET_PLAT_PS3;
      else if (STREQ(arg, "psv"))
        target_plat = HKSC_TARGET_PLAT_PSV;
      else if (STREQ(arg, "ps4") || STREQ(arg, "orbis"))
        target_plat = HKSC_TARGET_PLAT_ORBIS;
      else if (STREQ(arg, "xbox360") || STREQ(arg, "xenon"))
        target_plat = HKSC_TARGET_PLAT_XENON;
      else if (STREQ(arg, "xboxone") || STREQ(arg, "durango"))
        target_plat = HKSC_TARGET_PLAT_DURANGO;
      else if (STREQN(arg, "windows")) {
        optionalnumber = 1;
        target_plat = HKSC_TARGET_PLAT_WINDOWS;
        arg += sizeof("windows")-1;
        goto numsuffix;
      }
      else if (STREQN(arg, "win")) {
        optionalnumber = 1;
        target_plat = HKSC_TARGET_PLAT_WINDOWS;
        arg += sizeof("win")-1;
        goto numsuffix;
      }
      else if (STREQN(arg, "gnu")) {
        optionalnumber = 1;
        target_plat = HKSC_TARGET_PLAT_GNU;
        arg += sizeof("gnu")-1;
        goto numsuffix;
      }
      else if (STREQN(arg, "darwin")) {
        optionalnumber = 1;
        target_plat = HKSC_TARGET_PLAT_DARWIN;
        arg += sizeof("darwin")-1;
        goto numsuffix;
      }
      else { /* `-m<16|32|64>' */
        numsuffix:
        if (arg[0] == '1' && arg[1] == '6')
          target_ws = HKSC_TARGET_WS_16;
        else if (arg[0] == '3' && arg[1] == '2')
          target_ws = HKSC_TARGET_WS_32;
        else if (arg[0] == '6' && arg[1] == '4')
          target_ws = HKSC_TARGET_WS_64;
        else {
          if (!optionalnumber)
            usage("invalid argument given with '-m'");
        }
      }
    }
#endif /* HKSC_MULTIPLAT */
    else if (IS("-p") || IS("--parse")) {     /* parse only */
      dumping=0;
      explicit_dumping=0;
    }
    else if (HAS("-s"))     /* specify stripping level */
    {
#ifdef LUA_CODT6
      /* do nothing */
      (void)striplevel;
      if (argv[i][2] != '\0')
        usage(argv[i]);
#else /* !LUA_CODT6 */
      char *mode;
      if (striparg)
        usage("'-s' used multiple times");
      if (argv[i][2] == '\0') {
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
#endif /* LUA_CODT6 */
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
  if (compiling && decompiling) /* both compile and decompile mode specified? */
    usage("both '%s' and '%s' used; only one may be used per invokation",
          opt_compile, opt_decompile);
#endif /* HKSC_DECOMPILER */
  if (a && b) /* both source and binary mode specified? */
    usage("both '%s' and '%s' used; only one may be used per invokation",
          opt_a, opt_b);
  else if (a) {
    if (ignore_debug)
      warn_unused("--ignore-debug", "compiling");
    mode=HKSC_MODE_SOURCE;
  }
  else if (b)
    mode=HKSC_MODE_BINARY;
  if (striparg && !dumping)
    warn_unused("-s", "not dumping bytecode");
#ifdef LUA_CODT6
  debugfile_arg = (debugfile != NULL);
  profilefile_arg = (profilefile != NULL);
  if ((debugfile_arg || profilefile_arg) && !withdebug) {
    usage("'--with-debug' must be provided with '--debugfile' or "
          "'--callstackdb'");
  }
  if (striparg && withdebug)
    usage("'-s' cannot be used when '%s' is provided", opt_withdebug);
#endif /* LUA_CODT6 */
  return nfiles;
}

#define FUNCTION "(function()end)();"

static int hksc_dump_f(hksc_State *H, void *ud) {
  const char *filename = (const char *)ud;
  if (!listing && !dumping) {
    fprintf(stderr, "Successfully parsed `%s'\n", filename);
    return 0;
  }
  if (listing) {
    lua_print(H, listing > 1);
    if (!dumping)
      return 0;
  }
#ifdef HKSC_DECOMPILER
  if (decompiling) {
    if (lua_getmode(H) == HKSC_MODE_SOURCE && ignore_debug == 0)
      lua_setignoredebug(H, 0);
    return hksc_dump_decomp(H, filename);
  }
#endif /* HKSC_DECOMPILER */
  return hksc_dump_bytecode(H, filename);
}


/*
** parser loop function
*/
static int dofiles (hksc_State *H, int argc, char *argv[]) {
  int i, status, error = 0;
  for (i = 0; i < argc; i++) {
    error |= (status = hksI_parser_file(H, argv[i], hksc_dump_f, argv[i]));
    if (status) {
      if (status == LUA_ERRSYNTAX) {
        fprintf(stderr, "%s\n", lua_geterror(H));
        lua_clearerror(H); /* discharge the error message and keep going */
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
      error_multiple_inputs("--output");
#ifdef LUA_CODT6
    if (debugfile_arg)
      error_multiple_inputs("--debugfile");
    if (profilefile_arg)
      error_multiple_inputs("--callstackdb");
#endif /* LUA_CODT6 */
  }
  hksI_StateSettings(&settings);
#ifdef HKSC_MULTIPLAT
  settings.target_plat = target_plat;
  settings.target_ws = target_ws;
#endif /* HKSC_MULTIPLAT */
  H = hksI_newstate(&settings);
  if (H==NULL) fatal("cannot create state: not enough memory");
  for (i = 0; i < nprefixmaps; i++)
    lua_addprefixmap(H, file_prefix_maps[i]);
  lua_setmode(H, mode);
  lua_setintliteralsenabled(H,literals_enabled);
#ifdef LUA_CODT6
  if (dumping) {
    lua_onstartcycle(H, luacod_startcycle);
    lua_onendcycle(H, luacod_endcycle);
  }
  lua_setbytecodestrippinglevel(H,BYTECODE_STRIPPING_ALL);
  lua_setignoredebug(H, !withdebug);
#else /* !LUA_CODT6 */
  lua_setbytecodestrippinglevel(H,striplevel);
  lua_setignoredebug(H, ignore_debug);
#endif /* LUA_CODT6 */
/*  if (listing)
    dumpf = hksc_dump_l;
  else if (!dumping)
    dumpf = hksc_dump_p;
  else
    dumpf = hksc_dump_default;*/
  status = dofiles(H, argc, argv);
  hksI_close(H);
  return status ? EXIT_FAILURE : EXIT_SUCCESS;
}
