/* All logic to be processed at the start of an hksc translation unit.
   Makes sure hksc interacts smoothly with modded Lua.

   IMPORTANT: This file must be included in any hksc file BEFORE any non-system
   headers are included.  */

#ifndef HKSC_BEGIN_CODE_H
#define HKSC_BEGIN_CODE_H


/* need to include all hksc-versions of Lua headers first, so that the
   Lua versions never get processed if accidentally included */
/* Be mindful of inter-dependencies with headers */
/*#include "./lmem.h"
#include "./lzio.h"
#include "./lobject.h"
#include "./lparser.h"
#include "./lfunc.h"
#include "./ltable.h"
#include "./lstring.h"
#include "./lopcodes.h"
#include "./llex.h"
#include "./lcode.h"*/

#endif /* HKSC_BEGIN_CODE_H */
