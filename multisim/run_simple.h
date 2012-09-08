#ifndef _RUN_SIMPLE
#define _RUN_SIMPLE 1

#include "isa.h"

bool step_simple(const isa_t *isa, cpu_state_t *state, bool verbose);
void run_simple(int, char **);

#endif
