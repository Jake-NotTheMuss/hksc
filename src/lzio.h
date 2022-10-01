/*
** $Id: lzio.h,v 1.20 2005/04/25 19:24:10 roberto Exp roberto $
** Buffered streams
** See Copyright Notice in lua.h
*/


#ifndef lzio_h
#define lzio_h

#include "hksc_begin_code.h"

#include "lua.h"

#include "lmem.h"


#define EOZ  (-1)      /* end of stream */

typedef struct Zio ZIO;

#define char2int(c)  cast(int, cast(unsigned char, (c)))

#define zgetc(z)  (((z)->n--)>0 ?  char2int(*(z)->p++) : luaZ_fill(z))

typedef struct Mbuffer {
  char *buffer;
  size_t n;
  size_t buffsize;
} Mbuffer;

#define luaZ_initbuffer(buff) ((buff)->buffer = NULL, (buff)->buffsize = 0)

#define luaZ_buffer(buff)  ((buff)->buffer)
#define luaZ_sizebuffer(buff)  ((buff)->buffsize)
#define luaZ_bufflen(buff)  ((buff)->n)

#define luaZ_resetbuffer(buff) ((buff)->n = 0)


#define luaZ_resizebuffer(buff, size) \
  (luaM_reallocvector((buff)->buffer, (buff)->buffsize, size, char), \
  (buff)->buffsize = size)

#define luaZ_freebuffer(buff)  luaZ_resizebuffer(buff, 0)


LUAI_FUNC char *luaZ_openspace (Mbuffer *buff, size_t n);
LUAI_FUNC void luaZ_init (ZIO *z, lua_Reader reader,
                                        void *data);
LUAI_FUNC size_t luaZ_read (ZIO* z, void* b, size_t n);  /* read next n bytes */
LUAI_FUNC int luaZ_lookahead (ZIO *z);



/* --------- Private Part ------------------ */

struct Zio {
  size_t n;      /* bytes still unread */
  const char *p;    /* current position in buffer */
  lua_Reader reader;
  void* data;      /* additional data */
};


LUAI_FUNC int luaZ_fill (ZIO *z);

#endif
