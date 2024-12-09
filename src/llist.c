/*
** $Id: llist.c $
** Generic operations on linked lists
** See Copyright Notice in lua.h
*/

#include <string.h>  /* memcpy */

#define llist_c
#define LUA_CORE

#include "hksclua.h"

#include "llist.h"
#include "lmem.h"

#define Node GenericNode  /* Node already exists in lobject.h */

/* generic node form used by the functions below */
typedef struct Node {
  NodeHeader;
} Node;

/* set how many bytes a node bank should be */
#define NODE_BANK_TARGET (20*32)

void luaO_initlist_ (List *list, int node_size) {
  list->head = list->freehead = NULL;
  list->node_size = node_size;
  list->bank_size = (NODE_BANK_TARGET / node_size) * node_size;
}

void luaO_delnode (List *list, void *n) {
  Node *node = n, *freenode;
  if (node->next != NULL) {  /* copy next into node and free NEXT */
    Node *next = node->next;
    int f1 = node->flags, f2 = next->flags;
    memcpy(node, next, list->node_size);
    node->flags = f1, next->flags = f2;
    /* NEXT is now unreachable, add it to the free list */
    freenode = next;
  }
  else {  /* N is the last node, get the previous node and unlink it */
    if (node == list->head)
      list->head = NULL;
    else {
      Node *prev;
      for (prev = list->head; prev != node; prev = prev->next)
        lua_assert(prev != NULL);
      prev->next = NULL;
    }
    freenode = node;
  }
  freenode->next = list->freehead;
  list->freehead = freenode;
}

void *luaO_newnode (hksc_State *H, List *list) {
  Node *new_node;
  /* check for a free node already allocated */
  if (list->freehead != NULL) {
    new_node = list->freehead;
    list->freehead = cast(Node *, list->freehead)->next;
  }
  else {  /* allocate a new bank of nodes */
    int i, count = list->bank_size / list->node_size;
    Node *node;
    char *const p = luaM_malloc(H, list->bank_size);
    for (i = 1; i < count; i++) {
      node = cast(void *, p + i * list->node_size);
      node->next = cast(void *, p + (i - 1) * list->node_size);
      node->flags = 0;
    }
    node = cast(void *, p);
    node->next = list->freehead;
    nodesetflag(node, OWNER);  /* so that it can be freed afterward */
    /* all new nodes except 1 go to the free list */
    list->freehead = cast(void *, p + (count - 2) * list->node_size);
    new_node = cast(void *, p + (count - 1) * list->node_size);
  }
  new_node->next = list->head;
  list->head = new_node;
  return new_node;
}

#ifdef LUA_DEBUG
static void verifylist (Node *list) {
  Node *node;
  for (node = list; node != NULL; node = node->next)
    nodeclearflag(node, VISITED);
  for (node = list; node != NULL; node = node->next)
    nodesetflag_ac(node, VISITED);
}
#endif /* LUA_DEBUG */

static void freebanks (hksc_State *H, Node *list, int bank_size) {
  Node *node = list, *ownerlist = NULL;
  /* find all bank owner nodes and chain them together before freeing them, as
     currently, any node in a bank can point to any other node in any other
     bank, meaning all allocated blocks need to be intact until all owners can
     be found */
  while (node != NULL) {
    Node *next = node->next;
    if (nodegetflag(node, OWNER)) {
      node->next = ownerlist;
      ownerlist = node;
    }
    node = next;
  }
  /* free all owner nodes */
  lua_assert(ownerlist != NULL);
  node = ownerlist;
  do {
    Node *next = node->next;
    luaM_freemem(H, node, bank_size);
    node = next;
  } while (node != NULL);
}

void luaO_freelist (hksc_State *H, List *list) {
  list->head = luaO_concatlist(list->head, list->freehead);
#ifdef LUA_DEBUG
  verifylist(list->head);
#endif
  freebanks(H, list->head, list->bank_size);
  list->head = list->freehead = NULL;
}

void *luaO_concatlist (void *list1, void *list2) {
  Node *node;
  if (list1 == NULL)
    return list2;
  /* get the last node in list1 and chain it to list2 */
  for (node = list1; node->next != NULL; node = node->next)
    ;
  node->next = list2;
  return list1;
}
