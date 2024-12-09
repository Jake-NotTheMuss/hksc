/*
** $Id: lmember.h $
** macros for dealing with struct members
** See Copyright Notice in lua.h
*/

#ifndef lmember_h
#define lmember_h

#define sizeofmember(t,m)  sizeof(((t *)0)->m)

/*
** These macros declare enum struct members based on what is most convenient
** for the build: when debugging, it is a normal enum; otherwise, the type is
** optimized for space.
*/
#ifdef LUA_DEBUG
#define ENUM_BITFIELD(e, n, name) e name
#define ENUM_BYTE(e, name) e name
#else /* LUA_DEBUG */
#define ENUM_BITFIELD(e, n, name) unsigned int name : n
#define ENUM_BYTE(e, name) lu_byte name
#endif /* !LUA_DEBUG */

#endif /* lmember_h */
