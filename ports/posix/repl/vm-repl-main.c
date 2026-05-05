/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB
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
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Author: Nicolas Tsiftes <nicolas.tsiftes@ri.se>
 */

/*
 * POSIX REPL frontend: reads protocol frames on stdin and writes
 * responses on stdout. Frame format ([type:u8][len:u16-be][payload])
 * matches doc/repl-design.md and tools/repl/velox_repl/protocol.py.
 */

#ifdef VM_REPL_ENABLE

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vm.h"
#include "vm-log.h"
#include "vm-repl.h"

/* Frame types -- keep in sync with the design doc and the Python
   FrameType enum in tools/repl/velox_repl/protocol.py. */
#define FT_APPLY    0x01
#define FT_RUN      0x02
#define FT_RESET    0x03
#define FT_KILL     0x04
#define FT_IO_IN    0x05
#define FT_RESULT   0x10
#define FT_ERROR    0x11
#define FT_IO_OUT   0x12
#define FT_STATUS   0x13
#define FT_ACK      0x14

#define MAX_PAYLOAD 0xFFFF

/* Frame output goes here. We dup STDOUT_FILENO to this fd at startup
   and redirect stdout to stderr so VM_PRINTF (banner, debug, app
   `display`) doesn't corrupt the binary protocol stream. */
static int frame_out_fd;

static int
read_exactly(int fd, void *buf, size_t n)
{
  uint8_t *p = buf;
  size_t got = 0;
  while(got < n) {
    ssize_t r = read(fd, p + got, n - got);
    if(r == 0) {
      return got == 0 ? 0 : -1;  /* 0 = clean EOF, -1 = short read */
    }
    if(r < 0) {
      return -1;
    }
    got += (size_t)r;
  }
  return 1;
}

static int
write_all(int fd, const void *buf, size_t n)
{
  const uint8_t *p = buf;
  size_t sent = 0;
  while(sent < n) {
    ssize_t w = write(fd, p + sent, n - sent);
    if(w <= 0) {
      return 0;
    }
    sent += (size_t)w;
  }
  return 1;
}

static int
send_frame(uint8_t type, const void *payload, size_t len)
{
  uint8_t header[3];
  if(len > MAX_PAYLOAD) {
    return 0;
  }
  header[0] = type;
  header[1] = (uint8_t)((len >> 8) & 0xFF);
  header[2] = (uint8_t)(len & 0xFF);
  if(!write_all(frame_out_fd, header, sizeof(header))) {
    return 0;
  }
  if(len > 0 && !write_all(frame_out_fd, payload, len)) {
    return 0;
  }
  return 1;
}

static void
send_ack(uint16_t start_id)
{
  uint8_t payload[2];
  payload[0] = (uint8_t)((start_id >> 8) & 0xFF);
  payload[1] = (uint8_t)(start_id & 0xFF);
  send_frame(FT_ACK, payload, sizeof(payload));
}

static void
send_error(uint8_t error_type, const char *msg)
{
  size_t msg_len = msg != NULL ? strlen(msg) : 0;
  uint8_t buf[256];
  size_t total = 1 + msg_len;
  if(total > sizeof(buf)) {
    msg_len = sizeof(buf) - 1;
    total = 1 + msg_len;
  }
  buf[0] = error_type;
  if(msg_len > 0) {
    memcpy(buf + 1, msg, msg_len);
  }
  send_frame(FT_ERROR, buf, total);
}

static int
handle_apply(vm_program_t *program, const uint8_t *payload, size_t len)
{
  vm_expr_id_t entry_id = 0;
  vm_repl_status_t status = vm_repl_apply_delta(program, payload, len,
                                                &entry_id);
  if(status == VM_REPL_OK || status == VM_REPL_DUPLICATE) {
    send_ack(entry_id);
    return 1;
  }

  const char *msg;
  switch(status) {
  case VM_REPL_BAD_MAGIC:      msg = "bad magic"; break;
  case VM_REPL_BAD_VERSION:    msg = "unsupported version"; break;
  case VM_REPL_BAD_FORMAT:     msg = "malformed delta"; break;
  case VM_REPL_OUT_OF_SYNC:    msg = "delta out of sync"; break;
  case VM_REPL_OUT_OF_MEMORY:  msg = "out of memory"; break;
  case VM_REPL_LIMIT_EXCEEDED: msg = "limit exceeded"; break;
  default:                     msg = "delta error"; break;
  }
  send_error(VM_ERROR_BYTECODE, msg);
  return 0;
}

static uint8_t result_buf[MAX_PAYLOAD];

static void
handle_run(vm_program_t *program, const uint8_t *payload, size_t len)
{
  vm_expr_id_t entry_id;
  vm_obj_t result;
  vm_error_t error;
  size_t enc_len;

  if(len < 2) {
    send_error(VM_ERROR_BYTECODE, "RUN payload too short");
    return;
  }
  entry_id = (vm_expr_id_t)((payload[0] << 8) | payload[1]);
  memset(&error, 0, sizeof(error));

  if(!vm_repl_run(program, entry_id, &result, &error)) {
    send_error((uint8_t)error.error_type, "run failed");
    return;
  }

  enc_len = vm_repl_encode_obj(program, &result, result_buf,
                               sizeof(result_buf));
  if(enc_len == 0) {
    /* Result didn't fit; emit a NONE so the driver still gets a
       terminal frame for this turn. */
    result_buf[0] = 0x01;
    enc_len = 1;
  }
  send_frame(FT_RESULT, result_buf, enc_len);
}

static int
handle_reset(vm_program_t **program_slot, const vm_policy_t *policy)
{
  vm_program_t *new_program;
  if(*program_slot != NULL) {
    vm_repl_program_destroy(*program_slot);
    *program_slot = NULL;
  }
  new_program = vm_repl_program_create("repl", policy);
  if(new_program == NULL) {
    send_error(VM_ERROR_HEAP, "failed to recreate REPL program");
    return 0;
  }
  *program_slot = new_program;
  send_ack(0);
  return 1;
}

int
main(int argc, char *argv[])
{
  vm_program_t *program;
  uint8_t payload[MAX_PAYLOAD];

  (void)argc;
  (void)argv;

  /* Redirect VM_PRINTF (which writes to stdout) to stderr, and reserve
     the original stdout for the binary protocol so log output never
     corrupts a frame. */
  frame_out_fd = dup(STDOUT_FILENO);
  if(frame_out_fd < 0) {
    fprintf(stderr, "vm-repl: dup(stdout) failed\n");
    return EXIT_FAILURE;
  }
  if(dup2(STDERR_FILENO, STDOUT_FILENO) < 0) {
    fprintf(stderr, "vm-repl: dup2(stderr -> stdout) failed\n");
    return EXIT_FAILURE;
  }

  if(vm_init() == 0) {
    fprintf(stderr, "vm-repl: vm_init failed\n");
    return EXIT_FAILURE;
  }

  program = vm_repl_program_create("repl", NULL);
  if(program == NULL) {
    fprintf(stderr, "vm-repl: vm_repl_program_create failed\n");
    return EXIT_FAILURE;
  }

  for(;;) {
    uint8_t header[3];
    int r = read_exactly(STDIN_FILENO, header, sizeof(header));
    if(r == 0) {
      break;  /* clean EOF */
    }
    if(r < 0) {
      send_error(VM_ERROR_IO, "short read on header");
      break;
    }

    uint8_t type = header[0];
    size_t len = ((size_t)header[1] << 8) | header[2];

    if(len > 0) {
      r = read_exactly(STDIN_FILENO, payload, len);
      if(r <= 0) {
        send_error(VM_ERROR_IO, "short read on payload");
        break;
      }
    }

    switch(type) {
    case FT_APPLY:
      handle_apply(program, payload, len);
      break;
    case FT_RUN:
      handle_run(program, payload, len);
      break;
    case FT_RESET:
      handle_reset(&program, NULL);
      break;
    case FT_KILL:
      /* This frontend doesn't manage background threads;
         the ack keeps the driver in sync with the protocol. */
      send_ack(0);
      break;
    case FT_IO_IN:
      /* No application-input plumbing on this side; the bytes
         are dropped after the protocol-level ack. */
      break;
    default:
      send_error(VM_ERROR_BYTECODE, "unknown frame type");
      break;
    }
  }

  if(program != NULL) {
    vm_repl_program_destroy(program);
  }
  vm_exit();
  return EXIT_SUCCESS;
}

#endif /* VM_REPL_ENABLE */
