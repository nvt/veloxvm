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

%{
#include "iota-parser.tab.h"

#define INT_TO_STRING(base)                          \
  do {                                               \
    char tempbuf[128];                               \
    snprintf(tempbuf, sizeof(tempbuf), "%ld",        \
             strtoul(yytext, NULL, (base)));         \
    yylval.string = strdup(tempbuf);                 \
  } while(0)
#define STRING (yylval.string = strdup(yytext))
#define SYMBOL_TO_STRING                             \
  do {                                               \
    yylval.string = strdup(yytext + 1);              \
    yylval.string[strlen(yylval.string) - 1] = '\0'; \
  } while(0)
#define TOKEN (yylval.token = strdup(yytext))

/* Changes identifier names in scripts to their counterpart in Scheme. */
static void
translate_name(char *str)
{
  char *cp;

  for(cp = str; *cp != '\0'; cp++) {
    if(*cp == '_') {
      *cp = '-';
    }
  }
}
%}

%%
[ \t\n]		;
"/*"		{
                  int c;

                  do {
                    c = input();
                    if(c == '*') {
                      c = input();
                      if(c == '/') {
                        break;
                      }
                    }
                  } while(c != '\0' && c != EOF);
                }
"true"		return T_TRUE;
"false"		return T_FALSE;
"lambda"	return T_LAMBDA;
"function"	return T_FUNCTION;
"while"		return T_WHILE;
"for"		return T_FOR;
"if"		return T_IF;
"else"		return T_ELSE;
"try"		return T_TRY;
"catch"		return T_CATCH;
"def"		return T_DEF;
"case"		return T_CASE;
","             return T_COMMA;
"."		return T_DOT;
"{"             return T_LBRACE;
"}"             return T_RBRACE;
"("		return T_LPAREN;
")"		return T_RPAREN;
"["		return T_LBRACKET;
"]"		return T_RBRACKET;
"?"		return T_QUESTIONMARK;
":"		return T_COLON;
";"		return T_SEMICOLON;
"+"		return T_ADD;
"-"		return T_SUB;
"*"		return T_MUL;
"/"		return T_DIV;
"<"|"<="|">"|">="	{ TOKEN; return T_CMP_OP; };
"=="		return T_EQUAL;
"!="		return T_NOTEQUAL;
"&&"		return T_AND;
"||"		return T_OR;
"&"		return T_BITAND;
"|"		return T_BITOR;
"^"		return T_BITXOR;
"~"		return T_BITINVERT;
"!"		return T_BITNOT;
"<<"		return T_BITSHIFT_L;
">>"		return T_BITSHIFT_R;
"="		return T_ASSIGN;
"++"		return T_ASSIGN_ADD1;
"--"		return T_ASSIGN_SUBTRACT1;
"+="		return T_ASSIGN_ADD;
"-="		return T_ASSIGN_SUBTRACT;
"*="		return T_ASSIGN_MULTIPLY;
"/="		return T_ASSIGN_DIVIDE;
"<<="		return T_ASSIGN_BITSHIFT_L;
">>="		return T_ASSIGN_BITSHIFT_R;
\'.{1,2}\'	{ STRING; return T_CHAR; }
\"[^"]*\"	{ STRING; return T_STRING; }
0x[0-9a-fA-F]+	{ INT_TO_STRING(16); return T_RATIONAL; }
0[0-7]+		{ INT_TO_STRING(8); return T_RATIONAL; }
[0-9]+\/[0-9]+	{ STRING; return T_RATIONAL; }
[0-9]+\.[0-9]+	{ STRING; return T_FLOAT; }
-[0-9]+		{ STRING; return T_INTEGER; }
[0-9]+		{ STRING; return T_INTEGER; }
\*[[:alnum:]]+\*	{ SYMBOL_TO_STRING; return T_SYMBOL; }
[a-zA-Z0-9\_?<>$][a-zA-Z0-9\_?<>!$]*->[a-zA-Z0-9\_?<>!$]+	{ STRING; translate_name(yylval.string); return T_IDENTIFIER; }
[a-zA-Z0-9\_?<>$][a-zA-Z0-9\_?<>!$]*	{ STRING; translate_name(yylval.string); return T_IDENTIFIER; }
