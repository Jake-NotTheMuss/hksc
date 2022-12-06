/*
** $Id: lzio.h $
** Buffered streams
** See Copyright Notice in lua.h
*/


#ifndef lzio_h
#define lzio_h

#include "lua.h"

#include "lmem.h"


/*#define EOZ  (-1) */     /* end of stream */

/* stream states */
#define STREAM_OK 0
#define STREAM_END 1      /* end of stream */

typedef struct Zio ZIO;

#define char2int(c)  cast(int, cast(unsigned char, (c)))

#define zgetc(z)  (((z)->n--)>0 ?  char2int(*(z)->p++) : luaZ_fill(z))
#define zhasmore(z)  ((z)->state == STREAM_OK)

typedef struct Mbuffer {
  char *buffer;
  size_t n;
  size_t buffsize;
} Mbuffer;

#define luaZ_initbuffer(H, buff) ((buff)->buffer = NULL, (buff)->buffsize = 0)

#define luaZ_buffer(buff) ((buff)->buffer)
#define luaZ_sizebuffer(buff) ((buff)->buffsize)
#define luaZ_bufflen(buff)  ((buff)->n)

#define luaZ_resetbuffer(buff) ((buff)->n = 0)

#define luaZ_resizebuffer(H, buff, size) \
  (luaM_reallocvector(H, (buff)->buffer, (buff)->buffsize, size, char), \
  (buff)->buffsize = size)

#define luaZ_freebuffer(H, buff)  luaZ_resizebuffer(H, buff, 0)


LUAI_FUNC char *luaZ_openspace (hksc_State *H, Mbuffer *buff, size_t n);
LUAI_FUNC void luaZ_init (hksc_State *H, ZIO *z, lua_Reader reader,
                                        void *data);
LUAI_FUNC size_t luaZ_read (ZIO* z, void* b, size_t n); /* read next n bytes */
LUAI_FUNC int luaZ_lookahead (ZIO *z);



/* --------- Private Part ------------------ */

struct Zio {
  size_t n;      /* bytes still unread */
  const char *p;    /* current position in buffer */
  lua_Reader reader;
  void* data;      /* additional data */
  hksc_State *H;      /* Lua state (for reader) */
  int state;      /* stream state */
};


LUAI_FUNC int luaZ_fill (ZIO *z);

#endif
