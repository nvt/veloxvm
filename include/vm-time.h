/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES ARE DISCLAIMED.
 */

#ifndef VM_TIME_H
#define VM_TIME_H

#include "vm.h"

/* Returns truthy iff obj is the SRFI-18 time external object
   produced by current-time / seconds->time. */
int vm_time_check_type(const vm_obj_t *obj);

/* Convert a timeout-position argument into a relative millisecond
   wait. SRFI 18 timeouts are absolute deadlines; VeloxVM keeps an
   integer-ms-relative convention for back-compat. The accepted
   shapes are:

     - VM_TYPE_INTEGER: relative milliseconds (VeloxVM convention).
       Returned unchanged.
     - VM_TYPE_BOOLEAN: any value is treated as "no timeout"
       (#f is the SRFI 18 spelling) and returns -1.
     - SRFI-18 time external (vm_time_check_type true): absolute
       deadline. Returned as (deadline - now) in ms, clamped at 0
       if the deadline has already passed, and at INT32_MAX for very
       distant deadlines.

   *valid is set to 1 on success or 0 on any other arg type. The
   return value on failure is undefined. */
vm_integer_t vm_time_parse_timeout(const vm_obj_t *obj, int *valid);

#endif /* !VM_TIME_H */
