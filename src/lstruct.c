/*
** $Id: lstruct.c $
** Auxiliary functions to manipulate structure prototypes
** See Copyright Notice in lua.h
*/


#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define lstruct_c
#define LUA_CORE

#include "hksclua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "lstruct.h"
#include "ltable.h"


#if HKSC_STRUCTURE_EXTENSION_ON


static const char *const reserved_names[NUM_SLOTS_RESERVED] = {
  "meta",
  "proxytable",
  "__structure_internals__"
};


/*
** add a structure prototype to the global list in a Lua state
*/
static void growlist(hksc_State *H) {
  void *newblock;
  short newsize;
  StructProtoList *l = &G(H)->protolist;
  lua_assert(l->size <= LUAI_MAXSTRUCTS);
  if (l->size > l->nuse)
    return;  /* have anough space */
  if (l->size >= LUAI_MAXSTRUCTS/2)
    newsize = LUAI_MAXSTRUCTS;
  else
    newsize = l->size * 2;
  newblock = luaM_reallocv(H, l->list, l->size, newsize, sizeof(StructProto *));
  l->list = cast(StructProto **, newblock);
  l->size = newsize;
}


static void addtolist(hksc_State *H, StructProto *p) {
  global_State *g = G(H);
  growlist(H);
  lua_assert(g->protolist.nuse < g->protolist.size);
  g->protolist.list[g->protolist.nuse++] = p;
  p->id = g->protolist.nuse;
}


static void addtohashtable(hksc_State *H, Udata *u) {
  StructProto *p = cast(StructProto *, u+1);
  TValue key, *idx;
  setsvalue(&key, p->name);
  idx = luaH_set(H, G(H)->prototable, &key);
  setuvalue(idx, u);
}


static Udata *newproto(hksc_State *H, StructProto *proto) {
  global_State *g = G(H);
  Udata *u;
  if (g->protolist.nuse+1 >= LUAI_MAXSTRUCTS) {
    luaG_runerror(H, "too many structures defined");
    return NULL;
  }
  u = luaS_newproto(H, proto->nslots);
  u->uv.marked = luaC_white(g) | bitmask(FIXEDBIT);
  memcpy(getproto(u), proto, sizestruct(proto->nslots));
  addtolist(H, cast(StructProto *, u+1));
  return u;
}


/*
** register a new structure prototype in the Lua state
*/
StructProto *luaR_addstructproto(hksc_State *H, StructProto *proto) {
  /* create userdata for this struct and add it to the hashtable */
  Udata *u = newproto(H, proto);
  addtohashtable(H, u);
  proto = cast(StructProto *, u+1);
  { /* fix slots that use its own parent structure as its type */
    StructSlot *slots = getprotoslots(proto);
    size_t i;
    for (i = 0; i < proto->nslots; i++) {
      if (slots[i].structid == LAST_STRUCT_ID)
        slots[i].structid = proto->id;
    }
  }
  return proto;
}


static lu_byte getslotsize(hksc_State *H) {
  size_t size;
  (void)H;
#ifdef HKSC_MULTIPLAT
  if (Settings(H).target_ws != HKSC_TARGET_WS_DEFAULT) {
    int ws = Settings(H).target_ws;
    size = ws == HKSC_TARGET_WS_16 ? 2 : ws == HKSC_TARGET_WS_32 ? 4 : 8;
  }
  else
#endif /* HKSC_MULTIPLAT */
  size = sizeof(void *);
  return cast_byte(size);
}


lu_byte luaR_pos2index(hksc_State *H, lu_byte pos) {
  lu_byte size = getslotsize(H);  /* size of a slot */
  return pos / size * (size-1) - 1 + pos % size;
}


/* calculate the position of the nth slot */
lu_byte luaR_index2pos(hksc_State *H, lu_byte n) {
  lu_byte size = getslotsize(H);  /* size of a slot */
  return cast_byte(size * n / (size-1) + 1);
}


StructSlot *luaR_addslot(hksc_State *H, StructProto *proto, StructSlot *slots,
                         TString *name, int type, short structid) {
  StructSlot *slot = slots + proto->nslots;
  lua_assert(type >= 0);
  lua_assert(proto->nslots < MAX_STRUCT_SLOTS);
  slot->name = name;
  slot->index = cast_byte(proto->nslots);
  slot->structid = structid;
  slot->typeid = cast_byte(type);
  slot->reserved = 0;
  slot->position = luaR_index2pos(H, slot->index);
  proto->nslots++;
  return slot;
}


void luaR_addreservedslots(hksc_State *H, StructProto *proto, StructSlot *slots)
{
  /* add internal slot */
  StructSlot *slot = luaR_addslot(H, proto, slots,
                                  G(H)->slotnames[SLOT_RESERVED_INTERNAL],
                                  LUA_TNIL, 0);
  slot->reserved = SLOT_RESERVED_INTERNAL;
  slot->position = 0;  /* internal slot position is 0 */
  /* meta slot */
  slot = luaR_addslot(H, proto, slots, G(H)->slotnames[SLOT_RESERVED_META],
                      LUA_TNIL, 0);
  slot->reserved = SLOT_RESERVED_META;
  /* prpxy table slot */
  slot = luaR_addslot(H, proto, slots, G(H)->slotnames[SLOT_RESERVED_PROXY],
                      LUA_TTABLE, -1);
  slot->reserved = SLOT_RESERVED_PROXY;
}


StructSlot *luaR_findslot(StructProto *proto, TString *name) {
  size_t i;
  for (i = 0; i < proto->nslots; i++) {
    StructSlot *slot = getprotoslots(proto)+i;
    if (slot->name == name)
      return slot;
  }
  return NULL;
}


/*
** compare 2 structure prototypes
*/
int luaR_compareproto(StructProto *p1, StructSlot *s1, StructProto *p2,
                      StructSlot *s2) {
  size_t i;
  if (p1->nslots != p2->nslots)
    return 0;
  if (p1->hasmeta != p2->hasmeta)
    return 0;
  if (p1->hasproxy != p2->hasproxy)
    return 0;
  if (p1->name != p2->name)
    return 0;
  for (i = 0; i < p1->nslots; i++) {
    if (s1[i].name != s2[i].name)
      return 0;
    if (s1[i].structid != s2[i].structid)
      return 0;
    if (s1[i].typeid != s2[i].typeid)
      return 0;
    if (s1[i].reserved != s2[i].reserved)
      return 0;
    if (s1[i].index != s2[i].index)
      return 0;
    if (s2[i].position != s2[i].position)
      return 0;
  }
  return 1;
}


static StructProto *getloadedproto(hksc_State *H, Table *t, short id,
                                const char *protoname, const char *slotname) {
  TValue key;
  const TValue *res;
  setpvalue(&key, id2pvalue(id));
  res = luaH_get(t, &key);
  if (ttisnil(res)) {
    luaD_setferror(H, "Error when loading structure prototype '%s': "
                   "unknown structure type referenced from id (%d) for "
                    "slot '%s'", protoname, slotname, cast_int(id));
    return NULL;
  }
  return getproto(rawuvalue(res));
}


#define VM_L(c) ((c) ? "VM" : "loaded version")

#define slotname(s) \
  ((s)->reserved ? reserved_names[(s)->reserved-1] : getstr((s)->name))

static int check_struct_conflict(hksc_State *H, Table *t, StructProto *v,
                                 StructProto *l) {
  size_t i;
  const char *protoname;
  lua_assert(l != NULL);
  if (v == NULL)
    return 1;  /* no VM version */
  protoname = getstr(l->name);
  if (l->nslots != v->nslots) {
    luaD_setferror(H, "Conflict when loading structure prototype '%s': %d "
                   "slots in the loaded version, but %d slots in the VM",
                   protoname, cast_int(l->nslots), cast_int(v->nslots));
    return 0;
  }
  if (l->hasmeta != v->hasmeta) {
    luaD_setferror(H, "Conflict when loading structure prototype '%s': meta "
                   "slot exists in the %s, but not in the %s", protoname,
                   VM_L(v->hasmeta), VM_L(!v->hasmeta));
    return 0;
  }
  if (l->hasproxy != v->hasproxy) {
    luaD_setferror(H, "Conflict when loading structure prototype '%s': "
                   "proxytable option defined in the %s, but not in the %s",
                   protoname, VM_L(v->hasproxy), VM_L(!v->hasproxy));
    return 0;
  }
  for (i = 0; i < l->nslots; i++) {
    StructSlot *vmslot = getprotoslots(v)+i;
    StructSlot *loadedslot = getprotoslots(l)+i;
    if (vmslot->name != loadedslot->name) {
      luaD_setferror(H, "Conflict when loading structure prototype '%s': name "
                     "or slot order conflict; VM slot is '%s', loaded slot "
                "is '%s'", protoname, slotname(vmslot), slotname(loadedslot));
      return 0;
    }
    if (vmslot->typeid != loadedslot->typeid) {
      const char *vmtype, *loadedtype;
      if (vmslot->typeid == LUA_TSTRUCT)
        vmtype = getstr(luaR_getstructbyid(H, vmslot->structid)->name);
      else if (vmslot->typeid == LUA_TNIL)
        vmtype = luaX_typename(LUA_TANY);
      else
        vmtype = luaX_typename(vmslot->typeid);
      if (loadedslot->typeid == LUA_TSTRUCT) {
        StructProto *p = getloadedproto(H, t, loadedslot->structid, protoname,
                                        slotname(loadedslot));
        if (p == NULL) return 0;
        loadedtype = getstr(p->name);
      }
      else if (loadedslot->typeid == LUA_TNIL)
        vmtype = luaX_typename(LUA_TANY);
      else
        loadedtype = luaX_typename(loadedslot->typeid);
      luaD_setferror(H, "Conflict when loading structure prototype '%s': slot "
                     "'%s' defined with type '%s' in the VM, but with type "
                     "'%s' in the loaded version", protoname,
                     slotname(loadedslot), vmtype, loadedtype);
      return 0;
    }
    if (vmslot->reserved != loadedslot->reserved) {
      luaD_setferror(H, "Conflict when loading structure prototype '%s': slot "
                     "'%s' differs in internal definition. Are you using a "
                     "different Havok Script VM package?", protoname,
                     slotname(loadedslot));
      return 0;
    }
    if (vmslot->position != loadedslot->position) {
      luaD_setferror(H, "Conflict when loading structure prototype '%s': "
                     "structure layout differs on slot '%s'. Are you using a "
                     "different Havok Script VM package?", protoname,
                     slotname(loadedslot));
      return 0;
    }
    if (vmslot->typeid == LUA_TSTRUCT) {
      StructProto *vmproto = luaR_getstructbyid(H, vmslot->structid);
      StructProto *loadedproto = getloadedproto(H, t, loadedslot->structid,
                                              protoname, slotname(loadedslot));
      if (loadedproto == NULL) return 0;
      if (vmproto->name != loadedproto->name) {
        luaD_setferror(H, "Conflict when loading structure prototype '%s': "
                       "slot '%s' listed as containing structure '%s' in the "
                       "VM, and structure '%s' in the loaded version.",
                       protoname, slotname(loadedslot), getstr(vmproto->name),
                       getstr(loadedproto->name));
        return 0;
      }
    }
  }
  return 1;
}


/*
** called from lundump.c after loading structure prototypes
*/
int luaR_checkconflicts(hksc_State *H, Table *t) {
  TValue s[2];  /* stack for luaH_next */
  StkId key = &s[0], val = &s[1];
  setnilvalue(key);
  while (luaH_next(H, t, key)) {
    /* loaded and VM versions of the structure */
    StructProto *loadedproto, *vmproto;
    lua_assert(ttisuserdata(val));
    loadedproto = cast(StructProto *, rawuvalue(val)+1);
    vmproto = luaR_getstructbyname(H, loadedproto->name);
    if (!check_struct_conflict(H, t, vmproto, loadedproto))
      return 0;
  }
  return 1;
}


void luaR_mergeprototypes(hksc_State *H, Table *t) {
  global_State *g = G(H);
  /* a table to map loaded prototype ids to corresponding VM prototypes */
  Table *l2vm = luaH_new(H, 0, sizenode(t));
  TValue s[2];  /* stack for luaH_next */
  StkId key = &s[0], val = &s[1];
  setnilvalue(key);
  while (luaH_next(H, t, key)) {
    TValue *v;
    Udata *vmdata;
    StructProto *loadedproto = getproto(rawuvalue(val));
    StructProto *vmproto = luaR_getstructbyname(H, loadedproto->name);
    if (vmproto == NULL) {
      vmdata = newproto(H, loadedproto);
      vmproto = getproto(vmdata);
    }
    else {
      TValue k; const TValue *res;
      setsvalue(&k, vmproto->name);
      res = luaH_get(g->prototable, &k);
      vmdata = rawuvalue(res);
    }
    /* l2vm[loaded struct id] = VM struct proto */
    v = luaH_set(H, l2vm, key);
    setuvalue(v, vmdata);
    /* update the struct id for the loaded proto */
    loadedproto->id = vmproto->id;
  }
  setnilvalue(key);
  /* now that all struct prototypes are in the VM, go through each prototype and
     update any references to other structs with the new id of the referenced
     struct */
  while (luaH_next(H, t, key)) {
    StructProto *loadedproto = getproto(rawuvalue(val));
    StructProto *vmproto = luaR_getstructbyid(H, loadedproto->id);
    if (vmproto != NULL) {
      size_t i;
      for (i = 0; i < vmproto->nslots; i++) {
        StructSlot *slot = getprotoslots(vmproto)+i;
        if (slot->typeid == LUA_TSTRUCT) {
          TValue k;
          const TValue *v;
          setpvalue(&k, id2pvalue(slot->structid));
          v = luaH_get(t, &k);
          if (ttisuserdata(v))
            /* update the referenced struct id */
            slot->structid = getproto(rawuvalue(v))->id;
        }
      }
    }
  }
  setnilvalue(key);
  /* now go through and replace the userdata at each node with just the id */
  while (luaH_next(H, t, key)) {
    Udata *u = rawuvalue(val);
    StructProto *p = getproto(u);
    TValue *v = luaH_set(H, t, key);
    /* replace the value with just the struct id so that the userdata can be
       collected */
    setpvalue(v, id2pvalue(p->id));
    killtemp(obj2gco(u));  /* the load userdata can be collected */
  }
  setnilvalue(key);
  /* add the new prototypes to the global hash table */
  while (luaH_next(H, t, key)) {
    TValue k, *v = luaH_set(H, l2vm, key);
    Udata *vmdata = rawuvalue(v);
    setnilvalue(v);  /* clear the value in the mapping table */
    /* add the VM proto to the global hash table indexed by its name */
    setsvalue(&k, getproto(vmdata)->name);
    v = luaH_set(H, g->prototable, &k);
    if (ttisuserdata(v))
      /* if the VM already had this prototype before the load, VMDATA should
         point to the same userdata */
      lua_assert(rawuvalue(v) == vmdata);
    else {
      lua_assert(isfixed(g, obj2gco(vmdata)));
      setuvalue(v, vmdata);
    }
  }
  killtemp(obj2gco(l2vm));
}


/*
** search for a struct named NAME
*/
StructProto *luaR_getstructbyname(hksc_State *H, TString *name) {
  global_State *g = G(H);
  TValue key;
  const TValue *res;
  setsvalue(&key, name);
  res = luaH_get(g->prototable, &key);
  if (ttisuserdata(res)) {
    Udata *u = rawuvalue(res);
    return getproto(u);
  }
  return NULL;
}


/*
** search for a atruct with id ID
*/
StructProto *luaR_getstructbyid(hksc_State *H, short id) {
  global_State *g = G(H);
  if (id > 0 && id <= g->protolist.nuse)
    return g->protolist.list[id-1];
  return NULL;
}

#endif /* HKSC_STRUCTURE_EXTENSION_ON */
