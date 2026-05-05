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
 * CoAP frontend for the VeloxVM REPL.
 *
 * Two resources are exposed at startup:
 *
 *   POST /repl/cmd       Body is one driver->VM frame (APPLY/RUN/
 *                         RESET/KILL/INFO). Response body is the
 *                         immediate reply frame (ACK/INFO_REPLY/
 *                         ERROR), or 2.04 Changed with no body for
 *                         RUN since the value comes back via observe.
 *
 *   GET  /repl/events    Observable. Each notification carries one
 *                         async frame the VM produced (RESULT, ERROR,
 *                         IO_OUT, STATUS). Driver subscribes once
 *                         after connecting; the device pushes events
 *                         as evaluation progresses.
 *
 * Frames on the wire are the same byte format as the stdio transport:
 *   [type:u8][len:u16-be][payload]
 */

#ifdef VM_REPL_ENABLE

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "coap-engine.h"
#include "contiki.h"

#include "vm.h"
#include "vm-bytecode.h"
#include "vm-log.h"
#include "vm-native.h"
#include "vm-repl.h"

/* Frame types -- keep in sync with the design doc and the Python
   protocol module. */
#define FT_APPLY      0x01
#define FT_RUN        0x02
#define FT_RESET      0x03
#define FT_KILL       0x04
#define FT_IO_IN      0x05
#define FT_INFO       0x06
#define FT_RESULT     0x10
#define FT_ERROR      0x11
#define FT_IO_OUT     0x12
#define FT_STATUS     0x13
#define FT_ACK        0x14
#define FT_INFO_REPLY 0x15

#define VM_REPL_PROTOCOL_VERSION 1

#define CAP_IO_OUT  0x0001
#define CAP_IO_IN   0x0002
#define CAP_KILL    0x0004
#define CAP_STATUS  0x0008

#ifndef VM_REPL_BUILD_TAG
#define VM_REPL_BUILD_TAG __DATE__ " " __TIME__
#endif

/* Maximum payload bytes per outbound async frame. CoAP block-wise
   transfer would let us exceed this, but for a constrained device
   keeping each notification fit in a single datagram is simpler and
   matches what most observers can comfortably consume. */
#define EVENT_PAYLOAD_MAX 96

/* Depth of the outbound event queue. IO_OUT bursts during evaluation
   are the most common producer; a small ring covers the typical case.
   When full, new events are coalesced into the most recent IO_OUT or
   dropped (with an error log). */
#define EVENT_QUEUE_DEPTH 8

typedef struct {
  uint8_t type;
  uint16_t len;
  uint8_t bytes[EVENT_PAYLOAD_MAX];
} pending_frame_t;

static pending_frame_t event_queue[EVENT_QUEUE_DEPTH];
static unsigned eq_head;
static unsigned eq_tail;
static unsigned eq_count;

/* The single REPL program shared across all CoAP clients. */
static vm_program_t *repl_program;

/* Set between the start of a RUN turn and the next park; tells the
   poll loop to call vm_repl_collect each tick. */
static int turn_in_flight;

/* Scratch buffer for INFO_REPLY / RESULT serialization. Sized to fit
   the largest response payload comfortably; smaller ones reuse the
   prefix. */
static uint8_t scratch[256];

/* ------------------------------------------------------------------ */
/* Forward declarations                                               */
/* ------------------------------------------------------------------ */

static void cmd_post_handler(coap_message_t *request, coap_message_t *response,
                             uint8_t *buffer, uint16_t preferred_size,
                             int32_t *offset);
static void events_get_handler(coap_message_t *request, coap_message_t *response,
                               uint8_t *buffer, uint16_t preferred_size,
                               int32_t *offset);
static int io_out_writer(const char *bytes, size_t len);

PROCESS(vm_repl_coap_process, "vm-repl-coap");

/* ------------------------------------------------------------------ */
/* Resources                                                          */
/* ------------------------------------------------------------------ */

RESOURCE(res_repl_cmd,
         "title=\"REPL command\";rt=\"velox.cmd\"",
         NULL,
         cmd_post_handler,
         NULL,
         NULL);

EVENT_RESOURCE(res_repl_events,
               "title=\"REPL events\";obs;rt=\"velox.events\"",
               events_get_handler,
               NULL,
               NULL,
               NULL,
               NULL);

/* ------------------------------------------------------------------ */
/* Event queue                                                        */
/* ------------------------------------------------------------------ */

static int
enqueue_event(uint8_t type, const uint8_t *payload, size_t len)
{
  pending_frame_t *slot;

  if(len > EVENT_PAYLOAD_MAX) {
    len = EVENT_PAYLOAD_MAX;
  }

  if(eq_count >= EVENT_QUEUE_DEPTH) {
    /* Drop the oldest event so the most recent (which is usually a
       terminal RESULT or ERROR) wins. The driver will see fewer
       IO_OUT bytes than the application produced, but the turn
       structure stays intact. */
    eq_head = (eq_head + 1) % EVENT_QUEUE_DEPTH;
    eq_count--;
    VM_DEBUG(VM_DEBUG_LOW, "repl-coap: event queue overflow, dropped oldest");
  }

  slot = &event_queue[eq_tail];
  slot->type = type;
  slot->len = (uint16_t)len;
  if(len > 0) {
    memcpy(slot->bytes, payload, len);
  }
  eq_tail = (eq_tail + 1) % EVENT_QUEUE_DEPTH;
  eq_count++;

  coap_notify_observers(&res_repl_events);
  return 1;
}

/* ------------------------------------------------------------------ */
/* INFO_REPLY                                                         */
/* ------------------------------------------------------------------ */

static size_t
build_info_reply(uint8_t *out, size_t cap)
{
  static const char *const name = VM_NAME;
  static const char *const build = VM_REPL_BUILD_TAG;
  char version[16];
  size_t name_len = strlen(name);
  size_t version_len;
  size_t build_len = strlen(build);
  size_t pos = 0;
  uint16_t caps = CAP_IO_OUT;
  int n;

  n = snprintf(version, sizeof(version), "%d.%d",
               VM_VERSION_MAJOR, VM_VERSION_MINOR);
  version_len = (n > 0) ? (size_t)n : 0;

  if(name_len > 255) name_len = 255;
  if(version_len > 255) version_len = 255;
  if(build_len > 255) build_len = 255;

  if(cap < 4 + 1 + name_len + 1 + version_len + 1 + build_len) {
    return 0;
  }
  out[pos++] = VM_REPL_PROTOCOL_VERSION;
  out[pos++] = VM_BYTECODE_VERSION;
  out[pos++] = (uint8_t)((caps >> 8) & 0xFF);
  out[pos++] = (uint8_t)(caps & 0xFF);
  out[pos++] = (uint8_t)name_len;
  memcpy(out + pos, name, name_len);
  pos += name_len;
  out[pos++] = (uint8_t)version_len;
  memcpy(out + pos, version, version_len);
  pos += version_len;
  out[pos++] = (uint8_t)build_len;
  memcpy(out + pos, build, build_len);
  pos += build_len;
  return pos;
}

/* ------------------------------------------------------------------ */
/* Frame helpers                                                      */
/* ------------------------------------------------------------------ */

/* Emit a frame as the synchronous CoAP response payload.
   Layout: [type:u8][len:u16-be][bytes]. The driver's frame parser
   handles this directly in the response body. */
static void
send_response_frame(coap_message_t *response, uint8_t *buffer,
                    uint16_t cap, uint8_t type, const uint8_t *body,
                    size_t body_len)
{
  if(body_len > UINT16_MAX) {
    body_len = UINT16_MAX;
  }
  if(3 + body_len > cap) {
    /* Doesn't fit in the buffer; degrade to an empty ACK instead of
       silently truncating, so the driver gets a clean "something
       went wrong" rather than a malformed frame. */
    buffer[0] = FT_ERROR;
    buffer[1] = 0;
    buffer[2] = 1;
    buffer[3] = (uint8_t)VM_ERROR_BYTECODE;
    coap_set_payload(response, buffer, 4);
    return;
  }
  buffer[0] = type;
  buffer[1] = (uint8_t)((body_len >> 8) & 0xFF);
  buffer[2] = (uint8_t)(body_len & 0xFF);
  if(body_len > 0) {
    memcpy(buffer + 3, body, body_len);
  }
  coap_set_payload(response, buffer, 3 + body_len);
}

static void
send_response_ack(coap_message_t *response, uint8_t *buffer,
                  uint16_t cap, uint16_t start_id)
{
  uint8_t payload[2];
  payload[0] = (uint8_t)((start_id >> 8) & 0xFF);
  payload[1] = (uint8_t)(start_id & 0xFF);
  send_response_frame(response, buffer, cap, FT_ACK, payload,
                      sizeof(payload));
}

static void
send_response_error(coap_message_t *response, uint8_t *buffer,
                    uint16_t cap, uint8_t error_type, const char *msg)
{
  size_t msg_len = msg ? strlen(msg) : 0;
  uint8_t tmp[64];
  size_t total;
  if(msg_len > sizeof(tmp) - 1) {
    msg_len = sizeof(tmp) - 1;
  }
  tmp[0] = error_type;
  if(msg_len > 0) {
    memcpy(tmp + 1, msg, msg_len);
  }
  total = 1 + msg_len;
  send_response_frame(response, buffer, cap, FT_ERROR, tmp, total);
}

/* ------------------------------------------------------------------ */
/* Console writer hook                                                */
/* ------------------------------------------------------------------ */

static int
io_out_writer(const char *bytes, size_t len)
{
  /* Application output emitted during evaluation. Chunk into one or
     more IO_OUT events to fit the per-event payload cap. */
  size_t off = 0;
  while(off < len) {
    size_t chunk = len - off;
    if(chunk > EVENT_PAYLOAD_MAX) {
      chunk = EVENT_PAYLOAD_MAX;
    }
    enqueue_event(FT_IO_OUT, (const uint8_t *)bytes + off, chunk);
    off += chunk;
  }
  return (int)len;
}

/* ------------------------------------------------------------------ */
/* /repl/cmd POST handler                                             */
/* ------------------------------------------------------------------ */

static void
cmd_post_handler(coap_message_t *request, coap_message_t *response,
                 uint8_t *buffer, uint16_t preferred_size, int32_t *offset)
{
  const uint8_t *payload;
  int payload_len;
  uint8_t type;
  uint16_t body_len;
  const uint8_t *body;

  payload_len = coap_get_payload(request, &payload);
  if(payload_len < 3) {
    send_response_error(response, buffer, preferred_size,
                        VM_ERROR_BYTECODE, "frame header truncated");
    return;
  }

  type = payload[0];
  body_len = (uint16_t)((payload[1] << 8) | payload[2]);
  body = payload + 3;
  if(payload_len < 3 + (int)body_len) {
    send_response_error(response, buffer, preferred_size,
                        VM_ERROR_BYTECODE, "frame payload truncated");
    return;
  }

  switch(type) {
  case FT_INFO: {
    size_t n = build_info_reply(scratch, sizeof(scratch));
    if(n == 0) {
      send_response_error(response, buffer, preferred_size,
                          VM_ERROR_BYTECODE, "INFO_REPLY too large");
      return;
    }
    send_response_frame(response, buffer, preferred_size, FT_INFO_REPLY,
                        scratch, n);
    return;
  }
  case FT_APPLY: {
    vm_expr_id_t entry_id = 0;
    vm_repl_status_t s = vm_repl_apply_delta(repl_program, body, body_len,
                                             &entry_id);
    if(s == VM_REPL_OK || s == VM_REPL_DUPLICATE) {
      send_response_ack(response, buffer, preferred_size, entry_id);
    } else {
      const char *msg;
      switch(s) {
      case VM_REPL_BAD_MAGIC:      msg = "bad magic"; break;
      case VM_REPL_BAD_VERSION:    msg = "unsupported version"; break;
      case VM_REPL_BAD_FORMAT:     msg = "malformed delta"; break;
      case VM_REPL_OUT_OF_SYNC:    msg = "delta out of sync"; break;
      case VM_REPL_OUT_OF_MEMORY:  msg = "out of memory"; break;
      case VM_REPL_LIMIT_EXCEEDED: msg = "limit exceeded"; break;
      default:                     msg = "delta error"; break;
      }
      send_response_error(response, buffer, preferred_size,
                          VM_ERROR_BYTECODE, msg);
    }
    return;
  }
  case FT_RUN: {
    vm_expr_id_t entry_id;
    vm_error_t error;
    if(body_len < 2) {
      send_response_error(response, buffer, preferred_size,
                          VM_ERROR_BYTECODE, "RUN payload too short");
      return;
    }
    entry_id = (vm_expr_id_t)((body[0] << 8) | body[1]);
    memset(&error, 0, sizeof(error));
    if(!vm_repl_start(repl_program, entry_id, &error)) {
      send_response_error(response, buffer, preferred_size,
                          (uint8_t)error.error_type, "run-start failed");
      return;
    }
    turn_in_flight = 1;
    /* Immediate sync ACK; RESULT/ERROR will arrive via observe once
       the main thread parks. */
    send_response_ack(response, buffer, preferred_size, entry_id);
    /* Wake the poll process so it starts checking for park. */
    process_poll(&vm_repl_coap_process);
    return;
  }
  case FT_RESET: {
    vm_program_t *new_program;
    if(repl_program != NULL) {
      vm_repl_program_destroy(repl_program);
      repl_program = NULL;
    }
    new_program = vm_repl_program_create("repl", NULL);
    if(new_program == NULL) {
      send_response_error(response, buffer, preferred_size,
                          VM_ERROR_HEAP, "failed to recreate REPL program");
      return;
    }
    repl_program = new_program;
    turn_in_flight = 0;
    /* Drain the event queue so stale events don't reach the driver
       after the reset takes effect. */
    eq_head = eq_tail = eq_count = 0;
    send_response_ack(response, buffer, preferred_size, 0);
    return;
  }
  case FT_KILL:
    /* This frontend doesn't manage background threads on the
       device side; the ack just satisfies the protocol. */
    send_response_ack(response, buffer, preferred_size, 0);
    return;
  case FT_IO_IN:
    /* No application-input plumbing on the device side; the
       bytes are dropped after the protocol-level ack. */
    send_response_ack(response, buffer, preferred_size, 0);
    return;
  default:
    send_response_error(response, buffer, preferred_size,
                        VM_ERROR_BYTECODE, "unknown frame type");
    return;
  }
}

/* ------------------------------------------------------------------ */
/* /repl/events GET handler                                           */
/* ------------------------------------------------------------------ */

static void
events_get_handler(coap_message_t *request, coap_message_t *response,
                   uint8_t *buffer, uint16_t preferred_size,
                   int32_t *offset)
{
  size_t pos = 0;

  /* CoAP observe is a lossy stream: rapid updated_state calls collapse
     into one notification, so we pack as many queued frames as fit
     into each response. The driver parses multiple frames per body. */
  while(eq_count > 0) {
    pending_frame_t *f = &event_queue[eq_head];
    size_t need = 3 + f->len;
    if(pos + need > preferred_size) {
      break;  /* defer the rest to the next notification */
    }
    buffer[pos++] = f->type;
    buffer[pos++] = (uint8_t)((f->len >> 8) & 0xFF);
    buffer[pos++] = (uint8_t)(f->len & 0xFF);
    if(f->len > 0) {
      memcpy(buffer + pos, f->bytes, f->len);
      pos += f->len;
    }
    eq_head = (eq_head + 1) % EVENT_QUEUE_DEPTH;
    eq_count--;
  }

  coap_set_payload(response, buffer, pos);

  /* Anything left over (didn't fit) gets a follow-up notification. */
  if(eq_count > 0) {
    coap_notify_observers(&res_repl_events);
  }
}

/* ------------------------------------------------------------------ */
/* Polling: detect main thread park, emit RESULT/ERROR                */
/* ------------------------------------------------------------------ */

static void
emit_result(void)
{
  vm_obj_t result;
  vm_error_t error;
  int s = vm_repl_collect(repl_program, &result, &error);
  if(s == 0) {
    return;  /* still running */
  }
  turn_in_flight = 0;
  if(s == 1) {
    size_t n = vm_repl_encode_obj(repl_program, &result, scratch,
                                  sizeof(scratch));
    if(n == 0) {
      /* Result didn't fit; emit a NONE so the driver still gets a
         terminal frame. */
      scratch[0] = 0x01;
      n = 1;
    }
    enqueue_event(FT_RESULT, scratch, n);
  } else {
    /* Error. */
    uint8_t buf[2];
    buf[0] = (uint8_t)error.error_type;
    buf[1] = 0;
    enqueue_event(FT_ERROR, buf, 1);
  }
}

PROCESS_THREAD(vm_repl_coap_process, ev, data)
{
  static struct etimer poll_timer;

  PROCESS_BEGIN();

  while(1) {
    if(turn_in_flight) {
      /* Poll relatively often so RESULT lands quickly after park,
         but not so often that we spam the scheduler. The vm_process
         drives evaluation; we just check status here. */
      etimer_set(&poll_timer, CLOCK_SECOND / 10);
      PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&poll_timer) ||
                               ev == PROCESS_EVENT_POLL);
      emit_result();
    } else {
      PROCESS_WAIT_EVENT_UNTIL(ev == PROCESS_EVENT_POLL);
    }
  }

  PROCESS_END();
}

/* ------------------------------------------------------------------ */
/* Entry point invoked from vm.c on REPL builds                       */
/* ------------------------------------------------------------------ */

void
vm_repl_coap_init(vm_program_t *program)
{
  repl_program = program;
  vm_native_console_writer = io_out_writer;

  coap_activate_resource(&res_repl_cmd, "repl/cmd");
  coap_activate_resource(&res_repl_events, "repl/events");

  process_start(&vm_repl_coap_process, NULL);

  VM_DEBUG(VM_DEBUG_LOW,
           "REPL CoAP frontend ready: POST /repl/cmd, OBSERVE /repl/events");
}

#endif /* VM_REPL_ENABLE */
