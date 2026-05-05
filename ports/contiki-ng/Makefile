VM_PORT = Contiki-NG

ifndef TARGET
TARGET = zoul
endif

CONTIKI = ../../../contiki-ng

MODULES += os/net/app-layer/coap
MODULES += os/services/lwm2m
MODULES += os/services/resolv
MODULES += os/services/shell

# Application baked into the firmware as a RAM-resident bytecode image.
# Override on the command line, e.g. `make TARGET=zoul VM_APP=basic/factorial`.
# The path is interpreted relative to apps/<VM_APP>; the source can be in
# any front-end (.scm/.iscm/.cyl/.py) -- compile.sh figures it out.
VM_APP ?= basic/ticker

VM_APP_NAME = $(notdir $(VM_APP))
VM_APP_DIR  = $(dir $(VM_APP))
VM_APP_IMAGE = vm-app-image.h
REPO_ROOT = ../..
VM_APP_BYTECODE = $(REPO_ROOT)/apps/$(VM_APP_DIR)bin/$(VM_APP_NAME).vm
CREATE_RAM_MODULE = $(REPO_ROOT)/tools/create-ram-module

# VM_REPL=1 builds an interactive REPL firmware: no baked-in app
# image, the device exposes /repl/cmd and /repl/events CoAP resources,
# and the host-side velox-repl drives the VM over CoAP.
ifeq ($(VM_REPL),1)
all: vm
CFLAGS += -DVM_REPL_ENABLE
PROJECTDIRS_REPL = ../../core/repl repl
PROJECT_SOURCEFILES_REPL = ${notdir ${wildcard ../../core/repl/*.c repl/*.c}}
# On embedded targets the device's only viable path to the host-side
# driver is SLIP over the USB CDC interface. The rpl-border-router
# embedded module pulls in slip-bridge.c (which initialises the SLIP
# fallback interface and negotiates the IPv6 prefix with tunslip6).
# The native target reaches the host through its own tun/tap, so we
# skip it there.
ifneq ($(TARGET),native)
include $(CONTIKI)/Makefile.dir-variables
MODULES += $(CONTIKI_NG_SERVICES_DIR)/rpl-border-router
endif
else
all: $(VM_APP_IMAGE) vm
CFLAGS += -DVM_BUNDLE
PROJECTDIRS_REPL =
PROJECT_SOURCEFILES_REPL =
endif

# Regenerate the RAM-resident image whenever the underlying bytecode (or
# the embedding tool) changes. compile.sh handles the source-to-bytecode
# step; it is a no-op when the .vm is already up to date.
$(VM_APP_BYTECODE): FORCE
	@(cd $(REPO_ROOT) && ./compile.sh $(VM_APP))

$(CREATE_RAM_MODULE):
	$(MAKE) -C $(REPO_ROOT)/tools create-ram-module

# Re-derive the image on every make invocation so a change to VM_APP
# (which is not visible to make's mtime tracking) is reflected, but
# only touch the on-disk file if the content actually differs -- that
# keeps the linker idle when nothing has really changed.
$(VM_APP_IMAGE): $(VM_APP_BYTECODE) $(CREATE_RAM_MODULE) FORCE
	@$(CREATE_RAM_MODULE) $(VM_APP_BYTECODE) > $@.tmp
	@echo "const char vm_program_name[] = \"$(VM_APP_NAME).vm\";" >> $@.tmp
	@if cmp -s $@.tmp $@; then \
	  rm -f $@.tmp; \
	else \
	  echo "  EMBED     $(VM_APP) -> $@"; \
	  mv $@.tmp $@; \
	fi

# Force the bytecode rebuild to be evaluated each invocation -- compile.sh
# is the authoritative incrementality checker, not make's mtime tracking
# (the source might be a .py/.cyl that compiles to a different intermediate).
.PHONY: FORCE
FORCE:

CUSTOM_POLICY_FILE = ../../core/policies/vm-policy-defs-custom.c
DEFAULT_POLICY_FILE = ../../core/policies/vm-policy-defs-default.c

CFLAGS += -I../../include -DVM_PORT=$(VM_PORT) -DPROJECT_CONF_H=\"project-conf.h\"

PROJECTDIRS = ../../control ../../core ../../core/policies lib $(PROJECTDIRS_REPL)
PROJECT_SOURCEFILES = ${filter-out vm.c,${notdir ${wildcard ../../core/*.c ../../control/*.c *.c lib/*.c}}}
PROJECT_SOURCEFILES += $(PROJECT_SOURCEFILES_REPL)

ifeq ($(wildcard $(CUSTOM_POLICY_FILE)),)
	PROJECT_SOURCEFILES += ${notdir ${wildcard $(DEFAULT_POLICY_FILE)}}
else
	PROJECT_SOURCEFILES += ${notdir ${wildcard $(CUSTOM_POLICY_FILE)}}
endif

NEEDS_CONTIKI_VERSION_FILES += vm-native.c

include $(CONTIKI)/Makefile.include

# When VM_REPL=1 on an embedded target, the rpl-border-router/embedded
# module provides connect-router / connect-router-cooja targets that
# wire up tunslip6 with the right paths. We deliberately don't supply
# our own here so as not to override them with stale paths.
