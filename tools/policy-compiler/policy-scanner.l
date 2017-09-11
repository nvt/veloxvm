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
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

%option noyywrap
%option yylineno
%option caseless

%{
#include "policy-parser.tab.h"

extern void yyerror(const char *);

#define STRING (yylval.string = strdup(yytext))
#define TOKEN (yylval.token = strdup(yytext))

%}

%%
[ \t\n]		;
"#"		{
                  int c;

                  do {
                    c = input();
                  } while(c != '\n' && c != EOF);
                }
"PROGRAM-POLICY" return T_PROGRAM_POLICY;
"DEFAULT"        return T_DEFAULT;
"BANDWIDTH"      return T_BANDWIDTH;
"CPU"            return T_CPU;
"FILE"		 return T_FILE;
"MEMORY"         return T_MEMORY;
"NET"	         return T_NET;
"POWER"		 return T_POWER;
"RESOURCE"       return T_RESOURCE;
"THREADS"        return T_THREADS;
"READ"           return T_READ;
"WRITE"          return T_WRITE;
"RW"             return T_READWRITE;
"TCP"            return T_TCP;
"UDP"            return T_UDP;
"client"         return T_CLIENT;
"server"         return T_SERVER;
"kbps"           { STRING; return T_KBPS; }
"bps"            { STRING; return T_BPS; }
"window"         return T_WINDOW;
"throttle"       return T_THROTTLE;
"superuser"      return T_SUPERUSER;
"console"        return T_CONSOLE;
"DNS"            return T_DNS;
"IPC"            return T_IPC;
"stats"          return T_STATS;
"SHA256"         return T_SHA256;
";"              return T_SEMICOLON;
","              return T_COMMA;
"{"              return T_LBRACE;
"}"              return T_RBRACE;
"mW"             { STRING; return T_MW; }
"uW"             { STRING; return T_MW; }
[0-9]+		 { STRING; return T_INTEGER; }
[a-zA-Z0-9"/\-_:*%<>.]+ { STRING; return T_IDENTIFIER; }
