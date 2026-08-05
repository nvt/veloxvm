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

%{
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>

#define DEFAULT_VM_BASE_DIR "../../"
#define POLICY_DEFINITION_FILE "core/policies/vm-policy-defs-custom.c"

/* The values of these flags must match those defined in vm-policy.h. */
#define VM_POLICY_RESOURCE_SUPERUSER 0x01
#define VM_POLICY_RESOURCE_CONSOLE   0x02
#define VM_POLICY_RESOURCE_DNS       0x04
#define VM_POLICY_RESOURCE_IPC       0x08
#define VM_POLICY_RESOURCE_STATS     0x10

/* Mirror of vm_policy_reaction_t. USE_DEFAULT is the sentinel the
   loader resolves to the type-specific default. */
#define VM_POLICY_REACTION_EXCEPTION   0
#define VM_POLICY_REACTION_REPORT      1
#define VM_POLICY_REACTION_SLOWDOWN    2
#define VM_POLICY_REACTION_KILL        3
#define VM_POLICY_REACTION_USE_DEFAULT 4

#define ADD_RULE(fp) do {                                                  \
  fprintf((fp), "  rule.reaction = %s;\n",                                 \
          reaction_c_name(current_rule_reaction));                         \
  fprintf((fp), "  if(vm_policy_add_rule(p, &rule) == 0) {\n"              \
                "    return 0;\n  }\n\n");                                 \
} while(0)

extern int yylex();
extern int yylineno;
extern const char *yytext;
extern FILE *yyin;
extern void yyrestart(FILE *);

#define MAX_POLICY_CLASSES 32

struct policy_class {
  char *name;
  char *body;
  size_t body_size;
  uint8_t resources;
};

static char file_path[PATH_MAX];
static uint8_t resources;
static FILE *out_fp;

/* Reaction state. block_reaction is the default for every rule in the
   current { ... } block, settable with an ON-VIOLATION line.
   current_rule_reaction is what the next ADD_RULE emit will use; it is
   reset to block_reaction before each rule and overridden by a trailing
   per-rule ON-VIOLATION clause. */
static int block_reaction = VM_POLICY_REACTION_USE_DEFAULT;
static int current_rule_reaction = VM_POLICY_REACTION_USE_DEFAULT;

static struct policy_class classes[MAX_POLICY_CLASSES];
static int num_classes;

/* While a POLICY-CLASS block is being parsed, out_fp is redirected to a
   memory stream; the captured text becomes the class body. */
static FILE *saved_out_fp;
static char *class_buf;
static size_t class_buf_size;
static char *pending_class_name;

void
yyerror(const char *str)
{
  fprintf(stderr, "Unable to compile policy: %s\n", str);
  fprintf(stderr, "Line: %d\nToken: \"%s\"\n", yylineno, yytext);
  if(unlink(file_path) < 0) {
    perror("unlink");
  }
  exit(EXIT_FAILURE);
}

static void
add_resource(uint8_t resource)
{
  resources |= resource;
}

static const char *
reaction_c_name(int r)
{
  switch(r) {
  case VM_POLICY_REACTION_EXCEPTION: return "VM_POLICY_REACTION_EXCEPTION";
  case VM_POLICY_REACTION_REPORT:    return "VM_POLICY_REACTION_REPORT";
  case VM_POLICY_REACTION_SLOWDOWN:  return "VM_POLICY_REACTION_SLOWDOWN";
  case VM_POLICY_REACTION_KILL:      return "VM_POLICY_REACTION_KILL";
  default:                           return "VM_POLICY_REACTION_USE_DEFAULT";
  }
}

static void
block_begin(void)
{
  block_reaction = VM_POLICY_REACTION_USE_DEFAULT;
  current_rule_reaction = VM_POLICY_REACTION_USE_DEFAULT;
}

static void
class_begin(const char *name)
{
  saved_out_fp = out_fp;
  class_buf = NULL;
  class_buf_size = 0;
  out_fp = open_memstream(&class_buf, &class_buf_size);
  if(out_fp == NULL) {
    yyerror("open_memstream failed");
  }
  pending_class_name = strdup(name);
}

static void
class_end(void)
{
  fclose(out_fp);
  out_fp = saved_out_fp;
  if(num_classes >= MAX_POLICY_CLASSES) {
    yyerror("too many policy classes");
  }
  classes[num_classes].name = pending_class_name;
  classes[num_classes].body = class_buf;
  classes[num_classes].body_size = class_buf_size;
  classes[num_classes].resources = resources;
  num_classes++;
  resources = 0;
  pending_class_name = NULL;
  class_buf = NULL;
  class_buf_size = 0;
}

static void
class_expand(const char *name)
{
  int i;

  for(i = 0; i < num_classes; i++) {
    if(strcmp(classes[i].name, name) == 0) {
      fprintf(out_fp, "  /* INHERIT %s */\n", name);
      if(classes[i].body_size > 0) {
        fwrite(classes[i].body, 1, classes[i].body_size, out_fp);
      }
      resources |= classes[i].resources;
      return;
    }
  }
  fprintf(stderr, "Policy class \"%s\" not found (must be declared "
                  "before use)\n", name);
  yyerror("unknown policy class");
}
%}

%union {
  int token;
  const char *string;
}

%token <string> T_IDENTIFIER T_INTEGER
%token <string> T_BPS T_KBPS T_MW T_UW
%token <token> T_PROGRAM_POLICY T_DEFAULT T_BANDWIDTH T_CPU
%token <token> T_POWER T_FILE T_MEMORY
%token <token> T_NET T_RESOURCE T_THREADS
%token <token> T_READ T_WRITE T_READWRITE T_TCP T_UDP T_CLIENT T_SERVER
%token <token> T_WINDOW T_THROTTLE T_SHA256 T_LBRACE T_RBRACE T_SEMICOLON
%token <token> T_COMMA T_SUPERUSER T_CONSOLE T_DNS T_IPC T_STATS T_ANY
%token <token> T_POLICY_CLASS T_INHERIT
%token <token> T_ON_VIOLATION
%token <token> T_R_EXCEPTION T_R_REPORT T_R_SLOWDOWN T_R_KILL
%type <string> hash_value
%type <token> reaction_value

%%

policies: top_item
        | top_item policies
        ;

top_item: policy | policy_class;

policy: header T_LBRACE { block_begin(); } rules T_RBRACE
{
  if(resources != 0) {
    current_rule_reaction = block_reaction;
    fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_RESOURCES;\n");
    fprintf(out_fp, "  rule.resources.resource_access = %u;\n",
            (unsigned)resources);
    ADD_RULE(out_fp);
  }
  resources = 0;
};

policy_class: T_POLICY_CLASS T_IDENTIFIER T_LBRACE
              { class_begin($2); block_begin(); }
              rules T_RBRACE
{
  class_end();
};

rules: rule
     | rule rules
     ;

rule : bandwidth_rule
      | cpu_rule
      | file_rule
      | memory_rule
      | net_rule
      | power_rule
      | resource_rule
      | threads_rule
      | inherit_rule
      | violation_rule
       ;

inherit_rule: T_INHERIT T_IDENTIFIER
{
  class_expand($2);
};

violation_rule: T_ON_VIOLATION reaction_value
{
  block_reaction = $2;
  current_rule_reaction = $2;
};

reaction_value: T_R_EXCEPTION { $$ = VM_POLICY_REACTION_EXCEPTION; }
              | T_R_REPORT    { $$ = VM_POLICY_REACTION_REPORT; }
              | T_R_SLOWDOWN  { $$ = VM_POLICY_REACTION_SLOWDOWN; }
              | T_R_KILL      { $$ = VM_POLICY_REACTION_KILL; }
              ;

bandwidth_rule: T_BANDWIDTH T_INTEGER T_BPS
{
  current_rule_reaction = block_reaction;
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_BANDWIDTH;\n");
  fprintf(out_fp, "  rule.bandwidth.throughput = %d;\n", atoi($2));
  if(strcmp($3, "kbps") == 0) {
    fprintf(out_fp, "  rule.bandwidth.unit = VM_POLICY_UNIT_KBPS;\n");
  } else {
    fprintf(out_fp, "  rule.bandwidth.unit = VM_POLICY_UNIT_BPS;\n");
  }
  ADD_RULE(out_fp);
};

cpu_rule: T_CPU T_IDENTIFIER
{
  current_rule_reaction = block_reaction;
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_CPU;\n");
  fprintf(out_fp, "  rule.cpu.usage_percentage = %d;\n", atoi($2));
  fprintf(out_fp, "  rule.cpu.window = 0;\n");
  ADD_RULE(out_fp);
};

file_rule: T_FILE T_IDENTIFIER permission
{
  current_rule_reaction = block_reaction;
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_FILE;\n");
  fprintf(out_fp, "  rule.file.path = \"%s\";\n", $2);
  fprintf(out_fp, "  rule.file.flags = 0;\n");
  ADD_RULE(out_fp);
};

permission: T_READ | T_WRITE | T_READWRITE;

memory_rule: T_MEMORY T_INTEGER
{
  current_rule_reaction = block_reaction;
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_MEMORY;\n");
  fprintf(out_fp, "  rule.memory.limit = %d;\n", atoi($2));
  ADD_RULE(out_fp);
};

net_rule: T_NET T_ANY
{
  current_rule_reaction = block_reaction;
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_NET;\n");
  fprintf(out_fp, "  rule.net.address = NULL;\n");
  fprintf(out_fp, "  rule.net.port = 0;\n");
  ADD_RULE(out_fp);
}
        | T_NET T_IDENTIFIER T_INTEGER protocol direction
{
  current_rule_reaction = block_reaction;
  struct in6_addr address;
  int i;

  /* Convert the IPv6 address string to binary format. */
  errno = 0;
  switch(inet_pton(AF_INET6, $2, &address)) {
  case 1:
    /* Successful parsing of the IPv6 address. Continue to write the rule. */
    break;
  case 0:
    fprintf(stderr, "Cannot parse \"%s\" as an IPv6 address!\n", $2);
    exit(EXIT_FAILURE);
  case -1:
    fprintf(stderr, "Failed to parse address \"%s\": %s\n",
	    $2, strerror(errno));
    exit(EXIT_FAILURE);
  default:
    fprintf(stderr, "Unknown return value from inet_pton()!\n");
    exit(EXIT_FAILURE);
  }

  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_NET;\n");
  fprintf(out_fp, "  rule.net.address = (const unsigned char *)\"");
  for(i = 0; i < 16; i++) {
    fprintf(out_fp, "\\x%02x", address.s6_addr[i]);
  }
  fprintf(out_fp, "\";\n  rule.net.port = %d;\n", atoi($3));
  ADD_RULE(out_fp);
};

protocol: T_TCP | T_UDP;

direction: T_CLIENT | T_SERVER;

power_rule: T_POWER T_INTEGER T_UW |
            T_POWER T_INTEGER T_MW
{
  current_rule_reaction = block_reaction;
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_POWER;\n");
  fprintf(out_fp, "  rule.power.allocated_power = %d;\n", atoi($2));
  if(strcmp($3, "mW") == 0) {
    fprintf(out_fp, "  rule.power.unit = VM_POLICY_UNIT_MW;\n");
  } else {
    fprintf(out_fp, "  rule.power.unit = VM_POLICY_UNIT_UW;\n");
  }
  ADD_RULE(out_fp);
};

resource_rule:
  T_RESOURCE T_SUPERUSER { add_resource(VM_POLICY_RESOURCE_SUPERUSER); }
  | T_RESOURCE T_CONSOLE { add_resource(VM_POLICY_RESOURCE_CONSOLE); }
  | T_RESOURCE T_DNS { add_resource(VM_POLICY_RESOURCE_DNS); }
  | T_RESOURCE T_IPC { add_resource(VM_POLICY_RESOURCE_IPC); };
  | T_RESOURCE T_STATS { add_resource(VM_POLICY_RESOURCE_STATS); }

threads_rule: T_THREADS T_INTEGER
{
  current_rule_reaction = block_reaction;
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_THREADS;\n");
  fprintf(out_fp, "  rule.threads.limit = %d;\n", atoi($2));
  ADD_RULE(out_fp);
};

header: program_header | default_header;

program_header: T_PROGRAM_POLICY T_IDENTIFIER T_SHA256 hash_value
{
  fprintf(out_fp, "  /* Policy definition for program %s */\n", $2);
  fprintf(out_fp, "  p = vm_policy_add(\"%s\", NULL, %d);\n",
          $2, (int)(strlen($4) * 8));
  fprintf(out_fp, "  if(p == NULL) {\n    return 0;\n  }\n\n");
};

hash_value: T_IDENTIFIER | T_INTEGER;

default_header: T_DEFAULT
{
  fprintf(out_fp, "  /* Permissive fallback policy attached to programs"
                  " without a named policy. */\n");
  fprintf(out_fp, "  p = &vm_policy_default;\n\n");
};

%%
int
main(int argc, char *argv[])
{
  char *vm_base_dir;
  int i;

  if(argc < 2) {
    fprintf(stderr, "Usage: %s <policy-file> [<policy-file> ...]\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  vm_base_dir = getenv("VM_BASE_DIR");
  if(vm_base_dir == NULL) {
    printf("The environmental variable VM_BASE_DIR is not set, using %s\n",
            DEFAULT_VM_BASE_DIR);
    vm_base_dir = DEFAULT_VM_BASE_DIR;
  }

  snprintf(file_path, sizeof(file_path), "%s%s",
	   vm_base_dir, POLICY_DEFINITION_FILE);

  out_fp = fopen(file_path, "w");
  if(out_fp == NULL) {
    fprintf(stderr, "Failed to open the policy definition file \"%s\"\n",
            POLICY_DEFINITION_FILE);
    return EXIT_FAILURE;
  }

  printf("Writing policy definitions to \"%s\"\n", file_path);

  fprintf(out_fp, "#include <vm.h>\n\n");
  fprintf(out_fp, "#include <vm-policy.h>\n\n");
  fprintf(out_fp, "/* Customized policy definitions for the VM. */\n");
  fprintf(out_fp, "int\nvm_policy_define(void)\n{\n");
  fprintf(out_fp, "  vm_policy_t *p;\n");
  fprintf(out_fp, "  vm_policy_rule_t rule;\n\n");

  for(i = 1; i < argc; i++) {
    yyin = fopen(argv[i], "r");
    if(yyin == NULL) {
      fprintf(stderr, "Unable to open the policy file %s\n", argv[i]);
      fclose(out_fp);
      unlink(file_path);
      exit(EXIT_FAILURE);
    }
    fprintf(out_fp, "  /* === source: %s === */\n", argv[i]);
    yyrestart(yyin);
    yyparse();
    fclose(yyin);
  }

  fprintf(out_fp, "  return 1;\n}\n");

  return EXIT_SUCCESS;
}
