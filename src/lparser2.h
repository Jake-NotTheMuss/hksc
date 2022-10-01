#ifndef HKSC_PARSER_H
#define HKSC_PARSER_H

#include "hksc_begin_code.h"

int hksc_parser_init(void);
Proto *hksc_parser(ZIO *z, Mbuffer *buff);

#endif /* HKSC_PARSER_H */
