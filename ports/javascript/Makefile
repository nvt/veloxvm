all: vm_js

VM_PORT ?= javascript

VM_INCLUDE_DIR = ../../include

VM_BITCODE_FILE = vm.bc
VM_JAVASCRIPT_FILE = vm.js

VM_OBJ_DIR = obj
VM_BIN_DIR = bin

VM_INCLUDE_FILES = ${wildcard $(VM_INCLUDE_DIR)/*.h}

VM_CORE_DIR = ../../core
VM_POLICY_DIR = ../../core/policies
VM_CONTROL_DIR = ../../control
VM_PORT_DIR = ../../ports/$(VM_PORT)

VM_CORE_FILES = ${wildcard $(VM_CORE_DIR)/*.c}
VM_CORE_OBJECTS = ${patsubst $(VM_CORE_DIR)/%.c,$(VM_OBJ_DIR)/%.bc,$(VM_CORE_FILES)}

CUSTOM_POLICY_FILE = $(VM_POLICY_DIR)/vm-policy-defs-custom.c
DEFAULT_POLICY_FILE = $(VM_POLICY_DIR)/vm-policy-defs-default.c

ifeq ($(wildcard $(CUSTOM_POLICY_FILE)),)
        VM_POLICY_FILES = ${wildcard $(DEFAULT_POLICY_FILE)}
else
        VM_POLICY_FILES = ${wildcard $(CUSTOM_POLICY_FILE)}
endif
VM_POLICY_OBJECTS = ${patsubst $(VM_POLICY_DIR)/%.c,$(VM_OBJ_DIR)/%.bc,$(VM_POLICY_FILES)}


VM_CONTROL_FILES = ${wildcard $(VM_CONTROL_DIR)/*.c}
VM_CONTROL_OBJECTS = ${patsubst $(VM_CONTROL_DIR)/%.c,$(VM_OBJ_DIR)/%.bc,$(VM_CONTROL_FILES)}

VM_PORT_FILES = ${wildcard $(VM_PORT_DIR)/*.c}
VM_PORT_OBJECTS = ${patsubst $(VM_PORT_DIR)/%.c,$(VM_OBJ_DIR)/%.bc,$(VM_PORT_FILES)}

CFLAGS += -Wall -g -DVM_PORT=$(VM_PORT)
CFLAGS += -I$(VM_INCLUDE_DIR) -I$(VM_PORT_DIR)
CFLAGS += $(VM_PORT_CFLAGS)
CFLAGS += ${patsubst %,-I$(VM_PORT_DIR)/%,$(VM_PORT_INCLUDE_DIRS)}
CFLAGS += $(DEFINES)

VM_LIB_DIRS = ${patsubst %,-L$(VM_PORT_DIR)/%,$(VM_PORT_LIB_DIRS)}
VM_LIBS = ${patsubst %,-l%,$(VM_PORT_LIBS)}

$(VM_CORE_OBJECTS): $(VM_OBJ_DIR)/%.bc: $(VM_CORE_DIR)/%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

$(VM_POLICY_OBJECTS): $(VM_OBJ_DIR)/%.bc: $(VM_POLICY_DIR)/%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

$(VM_CONTROL_OBJECTS): $(VM_OBJ_DIR)/%.bc: $(VM_CONTROL_DIR)/%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

$(VM_PORT_OBJECTS): $(VM_OBJ_DIR)/%.bc: $(VM_PORT_DIR)/%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

$(VM_BIN_DIR):
	@mkdir -p $(VM_BIN_DIR)

$(VM_OBJ_DIR):
	@mkdir -p $(VM_OBJ_DIR)

vm_bc: $(VM_BIN_DIR) $(VM_OBJ_DIR) $(VM_CORE_OBJECTS) \
    $(VM_POLICY_OBJECTS) $(VM_CONTROL_OBJECTS) $(VM_PORT_OBJECTS)
	$(CC) $(CFLAGS) -o $(VM_BIN_DIR)/$(VM_BITCODE_FILE) \
		$(VM_CORE_OBJECTS) $(VM_POLICY_OBJECTS) \
		$(VM_CONTROL_OBJECTS) $(VM_PORT_OBJECTS) \
		$(VM_LIB_DIRS) $(VM_LIBS)

vm_js: vm_bc
	$(CC) $(VM_BIN_DIR)/$(VM_BITCODE_FILE) \
		-o $(VM_BIN_DIR)/$(VM_JAVASCRIPT_FILE)

clean:
	@rm -rf $(VM_BIN_DIR) $(VM_OBJ_DIR)
