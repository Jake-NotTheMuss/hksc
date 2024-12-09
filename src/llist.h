/*
** $Id: llist.h $
** Generic operations on linked lists
** See Copyright Notice in lua.h
*/

#ifndef llist_h
#define llist_h

#include "hksclua.h"

#include "lobject.h"

/*
** Place this macro at the top of the struct members for your node type. When
** the header fields change, use `grep NodeHeader' to find all places where
** member order may need to be adjusted in your node type to optimize space.
*/
#define NodeHeader void *next; lu_byte flags

/*
** Node flags, to be expanded on by inheriting structs
*/
enum NODE_FLAG {
  NODE_OWNER,
  NODE_VISITED,
  NODE_FIRST_EXTENDED_FLAG  /* the first bit available to inheriting structs */
};

#define nodegetflag(n,f)  (((n)->flags & (1u << NODE_##f)) != 0)
#define nodesetflag(n,f)  cast(void, (n)->flags |= (1u << NODE_##f))
#define nodeclearflag(n,f)  cast(void, (n)->flags &= ~(1u << NODE_##f))
/* set flag, assert clear */
#define nodesetflag_ac(n,f) check_exp(!nodegetflag(n,f), nodesetflag(n,f))


/*
** List - linked list structure
*/
typedef struct List {
  void *head, *freehead;
  int node_size;  /* size of a node (header + internal storage) */
  int bank_size;  /* calculated from node_size */
} List;

/*
** initialize a list with internal storage of node type T
*/
#define luaO_initlist(l, T)  luaO_initlist_(l, cast_int(sizeof(T)))
LUAI_FUNC void luaO_initlist_ (List *list, int node_size);

/*
** remove a node N from a list; N should not be accessed after calling
*/
LUAI_FUNC void luaO_delnode (List *list, void *n);

/*
** add a new node to a list, inserting it at the head, returning a pointer to
** the new node
*/
LUAI_FUNC void *luaO_newnode (hksc_State *H, List *list);

/*
** free all the nodes in a list and clear the head and freehead of the list
*/
LUAI_FUNC void luaO_freelist (hksc_State *H, List *list);

/*
** concatenate 2 lists, chaining list2 to the end of list1
*/
LUAI_FUNC void *luaO_concatlist (void *list1, void *list2);

#endif /* llist_h */
