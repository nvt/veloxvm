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

#define ADD_RULE(fp) \
  fprintf((fp), "  if(vm_policy_add_rule(p, &rule) == 0) {\n    return 0;\n  }\n\n");

/* The values of these flags must match those defined in vm-policy.h. */
#define VM_POLICY_RESOURCE_SUPERUSER 0x01
#define VM_POLICY_RESOURCE_CONSOLE   0x02
#define VM_POLICY_RESOURCE_DNS       0x04
#define VM_POLICY_RESOURCE_IPC       0x08
#define VM_POLICY_RESOURCE_STATS     0x10

extern int yylex();
extern int yylineno;
extern const char *yytext;
extern FILE *yyin;

static char file_path[PATH_MAX];
static uint8_t resources;
static FILE *out_fp;

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
%token <token> T_COMMA T_SUPERUSER T_CONSOLE T_DNS T_IPC T_STATS

%%

policies: policy
        | policy policies
        ;

policy: header T_LBRACE rules T_RBRACE
{
  if(resources != 0) {
    fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_RESOURCES;\n");
    fprintf(out_fp, "  rule.resources.resource_access = %u;\n",
            (unsigned)resources);
    ADD_RULE(out_fp);
  }
  resources = 0;
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
       ;

bandwidth_rule: T_BANDWIDTH T_INTEGER T_BPS
{
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
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_CPU;\n");
  fprintf(out_fp, "  rule.cpu.usage_percentage = %d;\n", atoi($2));
  fprintf(out_fp, "  rule.cpu.window = 0;\n");
  ADD_RULE(out_fp);
};

file_rule: T_FILE T_IDENTIFIER permission
{
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_FILE;\n");
  fprintf(out_fp, "  rule.file.path = \"%s\";\n", $2);
  fprintf(out_fp, "  rule.file.flags = 0;\n");
  ADD_RULE(out_fp);
};

permission: T_READ | T_WRITE | T_READWRITE;

memory_rule: T_MEMORY T_INTEGER
{
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_MEMORY;\n");
  fprintf(out_fp, "  rule.memory.limit = %d;\n", atoi($2));
  ADD_RULE(out_fp);
};

net_rule: T_NET T_IDENTIFIER T_INTEGER protocol direction
{
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
  fprintf(out_fp, "  rule.type = VM_POLICY_TYPE_THREADS;\n");
  fprintf(out_fp, "  rule.threads.limit = %d;\n", atoi($2));
  ADD_RULE(out_fp);
};

header: T_PROGRAM_POLICY T_IDENTIFIER T_SHA256 T_IDENTIFIER
{
  fprintf(out_fp, "  /* Policy definition for program %s */\n", $2);
  fprintf(out_fp, "  p = vm_policy_add(\"%s\", NULL, %d);\n",
          $2, (int)(strlen($4) * 8));
  fprintf(out_fp, "  if(p == NULL) {\n    return 0;\n  }\n\n");
};

%%
int
main(int argc, char *argv[])
{
  char *vm_base_dir;

  if(argc != 2) {
    fprintf(stderr, "Usage: %s <policy-file>\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  yyin = fopen(argv[1], "r");
  if(yyin == NULL) {
    fprintf(stderr, "Unable to open the policy file %s\n", argv[1]);
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

  yyparse();

  fprintf(out_fp, "  return 1;\n}\n");

  return EXIT_SUCCESS;
}
