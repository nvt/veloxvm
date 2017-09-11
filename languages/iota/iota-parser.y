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
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INTERNAL_SYMBOL_PREFIX "&"

extern int yylex();
extern int yylineno;
extern const char *yytext;
extern FILE *yyin;

/* For each new symbol that the compiler creates, we increase this counter
   to reduce the risk of collisions. */
static unsigned symbol_id_counter;

static void
yyerror(const char *str)
{
  fprintf(stderr, "Compilation error: %s\n", str);
  fprintf(stderr, "Line: %d\nToken: \"%s\"\n", yylineno, yytext);
  exit(EXIT_FAILURE);
}

/* Argument strings can be of the form "'\n'". This form has to be
   translated to the corresponding LISP form; e.g., "#\Newline". */
static char *
translate_char(const char *char_string)
{
#define CHAR_BUF_SIZE 16
  static char buf[CHAR_BUF_SIZE];
  size_t out_length;
  size_t in_length;

  in_length = strlen(char_string);
  out_length = 0;
  buf[out_length++] = '#';
  buf[out_length++] = '\\';
  if(in_length == 3) {
    if(char_string[1] == ' ') {
      out_length += snprintf(buf, CHAR_BUF_SIZE, "Space");
    } else {
      buf[out_length++] = char_string[1];
    }
  } else if(in_length == 4) {
    if(char_string[1] != '\\') {
      yyerror("X1");
    }
    switch(char_string[2]) {
    case 'n':
      /* Rewrite line feeds to newlines. */
      out_length += snprintf(&buf[out_length], CHAR_BUF_SIZE - out_length, "Linefeed");
      break;
    case 'r':
      /* Rewrite carriage returns to newlines. */
      out_length += snprintf(&buf[out_length], CHAR_BUF_SIZE - out_length, "Return");
      break;
    case 't':
      /* Rewrite tabs to spaces. */
      out_length += snprintf(&buf[out_length], CHAR_BUF_SIZE - out_length, "Space");
      break;
    default:
      if(char_string[2] >= '0' && char_string[2] <= '9') {
        buf[out_length++] = char_string[2];
      } else {
        yyerror("X2");
      }
    }
  } else {
    yyerror("X3");
  }

  buf[out_length] = '\0';
  return buf;
}

static char *
create_string(const char *format, ...)
{
  char *buf;
  char *adjusted_buf;
  va_list ap;
  int size;
#define MAX_LENGTH 32768

  buf = malloc(MAX_LENGTH);
  if(buf == NULL) {
    return NULL;
  }

  va_start(ap, format);
  size = vsnprintf(buf, MAX_LENGTH, format, ap);
  va_end(ap);

  adjusted_buf = realloc(buf, size + 1);
  if(adjusted_buf != NULL) {
    return adjusted_buf;
  }

  return buf;
}
%}

%union {
  char *token;
  char *string;
}

%expect 3

%token <string> T_IDENTIFIER T_RATIONAL T_FLOAT T_INTEGER T_STRING
%token <string> T_CHAR T_SYMBOL

%token <token> T_COMMA T_LPAREN T_RPAREN T_LBRACKET T_RBRACKET T_LBRACE T_RBRACE
%token <token> T_QUESTIONMARK T_COLON T_DOT T_SEMICOLON
%token <token> T_ADD T_SUB T_MUL T_DIV T_CMP_OP T_EQUAL T_NOTEQUAL
%token <token> T_AND T_OR T_BITAND T_BITOR T_BITXOR T_BITINVERT T_BITNOT
%token <token> T_BITSHIFT_L T_BITSHIFT_R T_ASSIGN T_TRY T_CATCH T_CASE
%token <token> T_LAMBDA T_FUNCTION T_TRUE T_FALSE T_IF T_ELSE T_WHILE T_FOR
%token <token> T_ASSIGN_ADD1 T_ASSIGN_SUBTRACT1 T_ASSIGN_ADD T_ASSIGN_SUBTRACT
%token <token> T_ASSIGN_MULTIPLY T_ASSIGN_DIVIDE
%token <token> T_ASSIGN_BITSHIFT_L T_ASSIGN_BITSHIFT_R
%token <token> T_DEF

%type <string> statements expr assignment vector_read list_def bit_op
%type <string> args arg_names lambda_def func_def func_call
%type <string> if_else if_else_extension create_pair
%type <string> case_default case_stmt case_list case
%type <string> catch_statements catch_statement exception_handler while_loop
%type <string> definition for_loop quoted_symbol

/* The relative precedence of operators is decided in the order in which
   they are declared. */
%left T_LBRACKET T_RBRACKET T_SEMICOLON
%left T_DOT
%left T_QUESTIONMARK T_COLON
%left T_AND T_OR
%left T_ASSIGN T_ASSIGN_ADD1 T_ASSIGN_SUBTRACT1 T_ASSIGN_ADD T_ASSIGN_SUBTRACT
%left T_ASSIGN_MULTIPLY T_ASSIGN_DIVIDE T_ASSIGN_BITSHIFT_L T_ASSIGN_BITSHIFT_R
%left T_BITINVERT T_BITNOT T_BITAND T_BITOR T_BITXOR T_BITSHIFT_L T_BITSHIFT_R
%left T_CMP_OP
%left T_ADD T_SUB
%left T_MUL T_DIV
%left T_EQUAL T_NOTEQUAL
%left T_DEF

%%

program : statements { printf("%s\n", $1); }

statements : while_loop statements { $$ = create_string("%s\n%s", $1, $2); }
           | while_loop { $$ = create_string("%s", $1); }
           | for_loop statements { $$ = create_string("%s\n%s", $1, $2); }
           | for_loop { $$ = create_string("%s", $1); }
           | exception_handler statements { $$ = create_string("%s\n%s", $1, $2); }
           | exception_handler { $$ = create_string("%s", $1); }
	   | case statements { $$ = create_string("%s\n%s", $1, $2); }
           | case { $$ = create_string("%s", $1); }
           | expr T_SEMICOLON statements { $$ = create_string("%s\n%s", $1, $3); }
           | expr T_SEMICOLON { $$ = create_string("%s", $1); }
	   | definition T_SEMICOLON statements { $$ = create_string("%s\n%s", $1, $3); }
           | definition T_SEMICOLON { $$ = create_string("%s", $1); }
           | if_else statements { $$ = create_string("%s\n%s", $1, $2); }
           | if_else { $$ = create_string("%s", $1); }
           | func_def statements { $$ = create_string("%s\n%s", $1, $2); }
           | func_def { $$ = create_string("%s", $1); }
           ;

definition : T_DEF T_IDENTIFIER T_ASSIGN expr { $$ = create_string("(DEFINE %s %s)", $2, $4); }
           | T_DEF T_IDENTIFIER { $$ = create_string("(DEFINE %s)", $2); }
           ;

arg_names : T_IDENTIFIER T_COMMA arg_names { $$ = create_string("%s %s", $1, $3); }
          | T_IDENTIFIER { $$ = create_string("%s", $1); }
          | { $$ = create_string("");}
          ;

args : expr T_COMMA args { $$ = create_string("%s %s", $1, $3); }
     | expr { $$ = create_string("%s", $1); }
     | { $$ = create_string(""); }
     ;

expr : assignment { $$ = create_string("%s", $1); }
     | func_call { $$ = create_string("%s", $1); }
     | lambda_def { $$ = create_string("%s", $1); }
     | list_def { $$ = create_string("%s", $1); }
     | bit_op { $$ = create_string("%s", $1); }
     | case { $$ = create_string("%s", $1); }
     | T_LPAREN expr T_RPAREN { $$ = create_string("%s", $2); }
     | T_LBRACE statements T_RBRACE { $$ = create_string("(begin %s)", $2); }
     | vector_read { $$ = create_string("%s", $1); }
     | T_RATIONAL { $$ = create_string("%s", $1); }
     | T_FLOAT { $$ = create_string("%s", $1); }
     | T_INTEGER { $$ = create_string("%s", $1); }
     | T_STRING { $$ = create_string("%s", $1); }
     | T_CHAR { $$ = create_string("%s", translate_char($1)); }
     | T_IDENTIFIER { $$ = create_string("%s", $1); }
     | quoted_symbol { $$ = create_string("%s", $1); }
     | T_TRUE { $$ = create_string("#t"); }
     | T_FALSE { $$ = create_string("#f"); }
     | create_pair { $$ = create_string("%s", $1); }
     | T_BITOR expr T_BITOR { $$ = create_string("(vector-length %s)", $2); }
     | T_IDENTIFIER T_ASSIGN_ADD1 { $$ = create_string("(set! %s (+ %s 1))", $1, $1); }
     | T_IDENTIFIER T_ASSIGN_SUBTRACT1 { $$ = create_string("(set! %s (- %s 1))", $1, $1); }
     | T_IDENTIFIER T_ASSIGN_ADD expr { $$ = create_string("(set! %s (+ %s %s))", $1, $1, $3); }
     | T_IDENTIFIER T_ASSIGN_SUBTRACT expr { $$ = create_string("(set! %s (- %s %s))", $1, $1, $3); }
     | T_IDENTIFIER T_ASSIGN_MULTIPLY expr { $$ = create_string("(set! %s (* %s %s))", $1, $1, $3); }
     | T_IDENTIFIER T_ASSIGN_DIVIDE expr { $$ = create_string("(set! %s (/ %s %s))", $1, $1, $3); }
     | T_IDENTIFIER T_ASSIGN_BITSHIFT_L expr { $$ = create_string("(set! %s (bit-shift %s %s))", $1, $1, $3); }
     | T_IDENTIFIER T_ASSIGN_BITSHIFT_R expr { $$ = create_string("(set! %s (bit-shift %s -%s))", $1, $1, $3); }
     | expr T_QUESTIONMARK expr T_COLON expr { $$ = create_string("(if %s %s %s)", $1, $3, $5); }
     | expr T_ADD expr { $$ = create_string("(+ %s %s)", $1, $3); }
     | expr T_SUB expr { $$ = create_string("(- %s %s)", $1, $3); }
     | expr T_MUL expr { $$ = create_string("(* %s %s)", $1, $3); }
     | expr T_DIV expr { $$ = create_string("(/ %s %s)", $1, $3); }
     | expr T_CMP_OP expr { $$ = create_string("(%s %s %s)", $2, $1, $3); }
     | expr T_AND expr { $$ = create_string("(and %s %s)", $1, $3); }
     | expr T_OR expr { $$ = create_string("(or %s %s)", $1, $3); }
     | expr T_EQUAL expr { $$ = create_string("(equal? %s %s)", $1, $3); }
     | expr T_NOTEQUAL expr { $$ = create_string("(not (equal? %s %s))", $1, $3); }
     ;

case_default: T_ELSE T_COLON expr T_SEMICOLON {$$ = create_string("(else %s)", $3); }

case_stmt: T_LPAREN args T_RPAREN T_COLON expr T_SEMICOLON {$$ = create_string("((%s) %s)", $2, $5); };

case_list: case_stmt case_list {$$ = create_string("%s %s", $1, $2); };
           | case_stmt case_default {$$ = create_string("%s %s", $1, $2); }
           | case_stmt {$$ = create_string("%s", $1); }

case: T_CASE T_LPAREN expr T_RPAREN T_LBRACE case_list T_RBRACE
    {
      $$ = create_string("(case %s %s)", $3, $6);
    };

quoted_symbol: T_SYMBOL { $$ = create_string("'%s", $1); }

exception_handler: T_TRY T_LBRACE statements T_RBRACE catch_statements
                 {
                   $$ = create_string("(guard (obj %s)\n%s)", $5, $3);
                 };

catch_statements: catch_statement catch_statements { $$ = create_string("%s %s", $1, $2); }
                | catch_statement { $$ = create_string("%s", $1); }
                ;

catch_statement: T_CATCH T_LPAREN quoted_symbol T_RPAREN T_LBRACE statements T_RBRACE
               {
                 $$ = create_string("((eq? obj %s) %s)", $3, $6);
               };

create_pair: expr T_DOT expr { $$ = create_string("(cons %s %s)", $1, $3); };

bit_op : expr T_BITAND expr { $$ = create_string("(bit-and %s %s)", $1, $3);}
       | expr T_BITOR expr { $$ = create_string("(bit-or %s %s)", $1, $3);}
       | expr T_BITXOR expr { $$ = create_string("(bit-xor %s %s)", $1, $3);}
       | expr T_BITSHIFT_L expr { $$ = create_string("(bit-shift %s %s)", $1, $3);}
       | expr T_BITSHIFT_R expr { $$ = create_string("(bit-shift %s -%s)", $1, $3);}
       | T_BITINVERT expr { $$ = create_string("(bit-invert %s)", $2); }
       | T_BITNOT expr { $$ = create_string("(not %s)", $2); }
       ;

list_def : T_LBRACKET args T_RBRACKET { $$ = create_string("(list %s)", $2); };

assignment : T_IDENTIFIER T_LBRACKET expr T_RBRACKET T_ASSIGN expr
             {
               $$ = create_string("(vector-set! %s %s %s)", $1, $3, $6);
             }
           | T_IDENTIFIER T_DOT T_IDENTIFIER T_ASSIGN expr
             {
               $$ = create_string("(set! %s (car %s))\n(set! %s (cdr %s))", $1, $5, $3, $5);
             }
           | expr T_ASSIGN expr
             {
               $$ = create_string("(set! %s %s)", $1, $3);
             }
           | T_IDENTIFIER T_ASSIGN expr
             {
               $$ = create_string("(set! %s %s)", $1, $3);
             }
           ;

vector_read: T_IDENTIFIER T_LBRACKET expr T_RBRACKET
             {
               $$ = create_string("(vector-ref %s %s)", $1, $3);
             }
           | T_LPAREN expr T_RPAREN T_LBRACKET expr T_RBRACKET
             {
               $$ = create_string("(vector-ref %s %s)", $2, $5);
             }
           ;

if_else : T_IF T_LPAREN expr T_RPAREN T_LBRACE statements T_RBRACE T_ELSE if_else_extension
          {
            $$ = create_string("(if %s (begin %s) (begin %s))", $3, $6, $9);
          }
        | T_IF T_LPAREN expr T_RPAREN T_LBRACE statements T_RBRACE
          {
            $$ = create_string("(if %s (begin %s))", $3, $6);
          }
        ;

if_else_extension : if_else { $$ = create_string("%s", $1); }
                  | T_LBRACE statements T_RBRACE { $$ = create_string("%s", $2); }
                  ;


func_def : T_FUNCTION T_IDENTIFIER T_LPAREN arg_names T_RPAREN T_LBRACE statements T_RBRACE
           {
             $$ = create_string("(define %s\n  (lambda (%s)\n    %s))\n", $2, $4, $7);
           }

lambda_def : T_LAMBDA T_LPAREN arg_names T_RPAREN T_LBRACE statements T_RBRACE
             {
               $$ = create_string("(lambda (%s) %s)", $3, $6);
             }
           ;

func_call : T_IDENTIFIER T_LPAREN args T_RPAREN
            {
              if(*($3) == '\0') {
                $$ = create_string("(%s)", $1);
              } else {
                $$ = create_string("(%s %s)", $1, $3);
              }
            }
          ;

for_loop: T_FOR T_LPAREN expr T_SEMICOLON expr T_SEMICOLON expr T_RPAREN T_LBRACE statements T_RBRACE
          {
            symbol_id_counter++;
            $$ = create_string("(let ((%s%x (lambda () (if %s (begin %s %s (%s%x))))))\n%s (%s%x))",
                               INTERNAL_SYMBOL_PREFIX, symbol_id_counter,
                               $5, $10, $7,
                               INTERNAL_SYMBOL_PREFIX, symbol_id_counter,
                               $3, INTERNAL_SYMBOL_PREFIX, symbol_id_counter);
          }

while_loop : T_WHILE T_LPAREN expr T_RPAREN T_LBRACE statements T_RBRACE
             {
               symbol_id_counter++;
               $$ = create_string("(let ((%s%x (lambda () (if %s (begin %s (%s%x)))))) (%s%x))",
                      INTERNAL_SYMBOL_PREFIX, symbol_id_counter, $3, $6,
                      INTERNAL_SYMBOL_PREFIX, symbol_id_counter,
                      INTERNAL_SYMBOL_PREFIX, symbol_id_counter);
             }
           ;

%%
int
main(int argc, char *argv[])
{
#if CSCRIPT_DEBUG
  yydebug = 1;
#endif

  if(argc <= 1) {
    yyin = stdin;
  } else {
    yyin = fopen(argv[1], "r");
    if(yyin == NULL) {
      fprintf(stderr, "Unable to open the source file %s\n", argv[1]);
      exit(EXIT_FAILURE);
    }
  }

  yyparse();

  return EXIT_SUCCESS;
}
