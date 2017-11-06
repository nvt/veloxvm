VM_PORT = Contiki-NG

ifndef TARGET
TARGET = zoul
endif

MODULES += os/net/app-layer/coap
MODULES += os/services/lwm2m

VM_APP_IMAGE = vm-app-image.h

all: check-app-image vm

check-app-image:
ifeq (,$(wildcard $(VM_APP_IMAGE)))
	$(error Run create-vm-in-ram.sh before making the VM firmware)
endif

CUSTOM_POLICY_FILE = ../../core/policies/vm-policy-defs-custom.c
DEFAULT_POLICY_FILE = ../../core/policies/vm-policy-defs-default.c

CFLAGS += -DVM_BUNDLE -I../../include -DVM_PORT=$(VM_PORT) -DPROJECT_CONF_H=\"project-conf.h\"

PROJECTDIRS = ../../control ../../core ../../core/policies
PROJECT_SOURCEFILES = ${filter-out vm.c,${notdir ${wildcard ../../core/*.c ../../control/*.c *.c}}}

ifeq ($(wildcard $(CUSTOM_POLICY_FILE)),)
	PROJECT_SOURCEFILES += ${notdir ${wildcard $(DEFAULT_POLICY_FILE)}}
else
	PROJECT_SOURCEFILES += ${notdir ${wildcard $(CUSTOM_POLICY_FILE)}}
endif

CONTIKI = ../../../contiki-ng
include $(CONTIKI)/Makefile.include

$(CONTIKI)/tools/tunslip6:	$(CONTIKI)/tools/tunslip6.c
	(cd $(CONTIKING)/tools && $(MAKE) tunslip6)

connect-router:	$(CONTIKI)/tools/tunslip6
	sudo $(CONTIKI)/tools/tunslip6 $(PREFIX)

connect-router-cooja:	$(CONTIKI)/tools/tunslip6
	sudo $(CONTIKI)/tools/tunslip6 -a 127.0.0.1 $(PREFIX)
