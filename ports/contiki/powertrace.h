/*
 * Copyright (c) 2012-2017, RISE SICS AB
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the Institute nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE INSTITUTE AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE INSTITUTE OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * This file is part of the Contiki operating system.
 *
 */

/**
 * \file
 *         Header file for the powertrace application
 * \author
 *         Adam Dunkels <adam@sics.se>
 */

#ifndef POWERTRACE_H
#define POWERTRACE_H

#include "sys/clock.h"

struct powertrace_sniff_stats {
  struct powertrace_sniff_stats *next;
  uint32_t num_input, num_output;
  uint32_t input_txtime, input_rxtime;
  uint32_t output_txtime, output_rxtime;
#if NETSTACK_CONF_WITH_IPV6
  uint16_t proto; /* includes proto + possibly flags */
#endif
  uint16_t channel;
  uint32_t last_input_txtime, last_input_rxtime;
  uint32_t last_output_txtime, last_output_rxtime;
};

void powertrace_start(clock_time_t period);
void powertrace_stop(void);

struct powertrace_sniff_stats *powertrace_get_channel_stats(uint16_t proto,
                                                            uint16_t channel);

typedef enum {
  POWERTRACE_ON,
  POWERTRACE_OFF
} powertrace_onoff_t;

void powertrace_sniff(powertrace_onoff_t onoff);

void powertrace_print(char *str);

#endif /* POWERTRACE_H */
