all: vm build_tools

VM_PORT ?= posix

APP_DIR = apps
TOOLS_DIR = tools
POLICY_COMPILER_DIR = $(TOOLS_DIR)/policy-compiler
LANGUAGES_DIR = languages
SCRIPT_DIR = $(LANGUAGES_DIR)/cyclus

.PHONY: build_tools

build_tools:
	@$(MAKE) -C $(TOOLS_DIR)
	@$(MAKE) -C $(POLICY_COMPILER_DIR)
	@$(MAKE) -C $(SCRIPT_DIR)

VM_INCLUDE_DIR = include

VM_EXECUTABLE_FILE = vm

VM_OBJ_DIR = obj
VM_BIN_DIR = bin

VM_INCLUDE_FILES = ${wildcard $(VM_INCLUDE_DIR)/*.h}

VM_CORE_DIR = core
VM_POLICY_DIR = core/policies
VM_CONTROL_DIR = control
VM_PORT_DIR = ports/$(VM_PORT)

include $(VM_PORT_DIR)/Makefile.include

VM_CORE_FILES = ${wildcard $(VM_CORE_DIR)/*.c}
VM_CORE_OBJECTS = ${patsubst $(VM_CORE_DIR)/%.c,$(VM_OBJ_DIR)/%.o,$(VM_CORE_FILES)}

CUSTOM_POLICY_FILE = $(VM_POLICY_DIR)/vm-policy-defs-custom.c
DEFAULT_POLICY_FILE = $(VM_POLICY_DIR)/vm-policy-defs-default.c

ifeq ($(wildcard $(CUSTOM_POLICY_FILE)),)
        VM_POLICY_FILES = ${wildcard $(DEFAULT_POLICY_FILE)}
else
        VM_POLICY_FILES = ${wildcard $(CUSTOM_POLICY_FILE)}
endif
VM_POLICY_OBJECTS = ${patsubst $(VM_POLICY_DIR)/%.c,$(VM_OBJ_DIR)/%.o,$(VM_POLICY_FILES)}


VM_CONTROL_FILES = ${wildcard $(VM_CONTROL_DIR)/*.c}
VM_CONTROL_OBJECTS = ${patsubst $(VM_CONTROL_DIR)/%.c,$(VM_OBJ_DIR)/%.o,$(VM_CONTROL_FILES)}

VM_PORT_FILES = ${wildcard $(VM_PORT_DIR)/*.c}
VM_PORT_OBJECTS = ${patsubst $(VM_PORT_DIR)/%.c,$(VM_OBJ_DIR)/%.o,$(VM_PORT_FILES)}

CFLAGS += -Wall -g -DVM_PORT=$(VM_PORT)
CFLAGS += -I$(VM_INCLUDE_DIR) -I$(VM_PORT_DIR)
CFLAGS += $(VM_PORT_CFLAGS)
CFLAGS += ${patsubst %,-I$(VM_PORT_DIR)/%,$(VM_PORT_INCLUDE_DIRS)}
CFLAGS += $(DEFINES)

VM_LIB_DIRS = ${patsubst %,-L$(VM_PORT_DIR)/%,$(VM_PORT_LIB_DIRS)}
VM_LIBS = ${patsubst %,-l%,$(VM_PORT_LIBS)}

$(VM_CORE_OBJECTS): $(VM_OBJ_DIR)/%.o: $(VM_CORE_DIR)/%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

$(VM_POLICY_OBJECTS): $(VM_OBJ_DIR)/%.o: $(VM_POLICY_DIR)/%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

$(VM_CONTROL_OBJECTS): $(VM_OBJ_DIR)/%.o: $(VM_CONTROL_DIR)/%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

$(VM_PORT_OBJECTS): $(VM_OBJ_DIR)/%.o: $(VM_PORT_DIR)/%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

$(VM_BIN_DIR):
	@mkdir -p $(VM_BIN_DIR)

$(VM_OBJ_DIR):
	@mkdir -p $(VM_OBJ_DIR)

vm: $(VM_BIN_DIR) $(VM_OBJ_DIR) $(VM_CORE_OBJECTS) \
    $(VM_POLICY_OBJECTS) $(VM_CONTROL_OBJECTS) $(VM_PORT_OBJECTS)
	$(CC) $(CFLAGS) -o $(VM_BIN_DIR)/$(VM_EXECUTABLE_FILE) \
		$(VM_CORE_OBJECTS) $(VM_POLICY_OBJECTS) \
		$(VM_CONTROL_OBJECTS) $(VM_PORT_OBJECTS) \
		$(VM_LIB_DIRS) $(VM_LIBS)

docker:
	sudo docker build -f tools/docker/Dockerfile -t veloxvm .

clean:
	rm -rf $(VM_OBJ_DIR)
	@$(MAKE) -C $(TOOLS_DIR) clean
	@$(MAKE) -C $(POLICY_COMPILER_DIR) clean
	@$(MAKE) -C $(SCRIPT_DIR) clean

cleanapps:
	@rm -rf $(VM_BIN_DIR) $(VM_OBJ_DIR)
	@rm -rf $(APP_DIR)/*.iscm
	@rm -rf $(APP_DIR)/*.vm

distclean: clean cleanapps
