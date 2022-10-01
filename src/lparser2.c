
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hksc_begin_code.h"
#define LUA_CORE
#include "lstring.h"

stringtable hksc_strt = {0};

Proto *hksc_parser(ZIO *z, Mbuffer *buff)
{
  return NULL;
}

int
hksc_parser_init(void)
{
  luaS_resize(MINSTRTABSIZE);  /* initial size of string table */
  /*luaX_init();*/

  if (hksc_strt.hash == NULL)
    return 1;
  else
    return 0;
}
