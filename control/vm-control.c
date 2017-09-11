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
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

#include "vm.h"
#include "vm-control.h"

#if VM_SERVER

#include "vm-file.h"
#include "vm-log.h"

#include "lwm2m-engine.h"

/* The ID for the VM control object. */
#ifndef VM_LWM2M_CONTROL_ID
#define VM_LWM2M_CONTROL_ID 6400
#endif

/* The ID for the VM application object. */
#ifndef VM_LWM2M_APP_ID
#define VM_LWM2M_APP_ID 6401
#endif

static lwm2m_object_instance_t control_object;

#define MAX_APP_INSTANCE_ID 10
static vm_program_t *app_instance_map[MAX_APP_INSTANCE_ID + 1];
static lwm2m_object_instance_t app_objects[MAX_APP_INSTANCE_ID + 1];

static vm_program_t *
get_app_from_context(lwm2m_context_t *ctx)
{
  if(ctx->object_instance_id >= MAX_APP_INSTANCE_ID) {
    return 0;
  }

  return app_instance_map[ctx->object_instance_id];
}

static unsigned
app_count(void)
{
  unsigned i;
  vm_program_t *program;

  for(i = 0, program = vm_get_programs();
      program != NULL;
      program = program->next, i++);

  return i;
}

static int
control_callback(lwm2m_object_instance_t *object, lwm2m_context_t *ctx)
{
  if(ctx->object_instance_id != 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Invalid LWM2M control instance: %d",
	     ctx->object_instance_id);
    return 0;
  }

  return 1;
}

static int
app_callback(lwm2m_object_instance_t *object, lwm2m_context_t *ctx)
{
  vm_program_t *program;

  program = get_app_from_context(ctx);
  if(program == NULL) {
    VM_DEBUG(VM_DEBUG_LOW, "Invalid LWM2M app instance: %d",
	     ctx->object_instance_id);
    return 0;
  }

  switch(ctx->resource_id) {
  case 8103:
    lwm2m_object_write_int(ctx, app_count());
    break;
  case 8104:
    /* TODO: Add write-app and exec-app here. */
    break;
  default:
    break;
  }

  return 1;
}

static int
lwm2m_callback(lwm2m_object_instance_t *object, lwm2m_context_t *ctx)
{
  VM_DEBUG(VM_DEBUG_LOW, "LW2M request regarding %d/%d/%d, level %d",
	   ctx->object_id, ctx->object_instance_id, ctx->resource_id,
	   ctx->level);

  if(ctx->level != 3) {
    VM_DEBUG(VM_DEBUG_LOW, "Unhandled LWM2M level: %d", ctx->level);
    return 0;
  }

  switch(ctx->object_id) {
  case VM_LWM2M_CONTROL_ID:
    return control_callback(object, ctx);
  case VM_LWM2M_APP_ID:
    return app_callback(object, ctx);
  default:
    VM_DEBUG(VM_DEBUG_LOW, "LWM2M request for an unknown object: %d",
	     ctx->object_id);
  }

  return 0;
}

#if 0
static int
exec_app(lwm2m_context_t *ctx, const uint8_t *buffer, size_t len,
         uint8_t *outbuf, size_t outlen)
{
  char name[len + 1];

  memcpy(name, buffer, len);
  name[len] = '\0';

  VM_DEBUG(VM_DEBUG_LOW, "Received request to load %s", name);
  return vm_load_program(name);
}

static int
write_app(lwm2m_context_t *ctx, const uint8_t *buffer, size_t len,
         uint8_t *outbuf, size_t outlen)
{
#define TMP_NAME "app0"

#if 0
  int fd;

  VM_DEBUG(VM_DEBUG_LOW, "Received request to upload app %s of %d bytes",
	   TMP_NAME, (int)len);

  fd = vm_file_open(TMP_NAME, VM_FILE_WRITE);
  if(fd < 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to open the file %s", TMP_NAME);
    return 0;
  }

  if(vm_file_write(fd, buffer, len) != (int)len) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to write to the file %s", TMP_NAME);
    vm_file_close(fd);
    return 0;
  }

  vm_file_close(fd);
  return 1;
#endif
  return 0;
}
#endif

int
vm_control_register_app(const vm_program_t *program)
{
  int i;
  lwm2m_object_instance_t *app_obj;

  VM_DEBUG(VM_DEBUG_LOW, "Register LWM2M app instance for %s", program->name);

  for(i = 0; i < MAX_APP_INSTANCE_ID; i++) {
    if(app_instance_map[i] == NULL) {
      /* Found a free slot. */
      app_obj = &app_objects[i];
      app_obj->object_id = VM_LWM2M_APP_ID;
      app_obj->instance_id = 0;
      app_obj->callback = lwm2m_callback;

      lwm2m_engine_add_object(app_obj);

      return 1;
    }
  }

  VM_DEBUG(VM_DEBUG_LOW, "No more room for LWM2M app instances");
  return 0;
}

int
vm_control_unregister_app(const vm_program_t *program)
{
  return 1;
}

int
vm_control_init(void)
{
  if(VM_SERVER) {
    VM_DEBUG(VM_DEBUG_LOW, "Initializing the LWM2M control object");

    lwm2m_engine_init();

    control_object.object_id = VM_LWM2M_CONTROL_ID;
    control_object.instance_id = 0;
    control_object.callback = lwm2m_callback;

    lwm2m_engine_add_object(&control_object);
  }

  return 1;
}

#else /* VM_SERVER */

int
vm_control_register_app(const vm_program_t *program)
{
  return 1;
}

int
vm_control_unregister_app(const vm_program_t *program)
{
  return 1;
}

int
vm_control_init(void)
{
  return 1;
}

#endif /* VM_SERVER */
