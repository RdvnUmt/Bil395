%{
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
int yylex(void);
void yyerror(char *s) { }
%}

%union {
    double doublevalue;
}

%token <doublevalue> DOUBLE
%token PLUS MINUS TIMES DIVIDE POWER LPAREN RPAREN NEWLINE

%type <doublevalue> expr

%left PLUS MINUS
%left TIMES DIVIDE
%right POWER

%%

program:
    | program line
    ;

line:
    NEWLINE
    | expr NEWLINE { printf("Islem sonucu: %.4g\n", $1); }
    ;

expr:
    expr PLUS expr   { $$ = $1 + $3; }
    | expr MINUS expr   { $$ = $1 - $3; }
    | expr TIMES expr   { $$ = $1 * $3; }
    | expr DIVIDE expr  { 
        if ($3 == 0) {
            printf("Error: Sifira bolme hatasi\n");
            $$ = 0;
        } else {
            $$ = $1 / $3;
        }
    }
    | expr POWER expr   { $$ = pow($1, $3); }
    | LPAREN expr RPAREN  { $$ = $2; }
    | DOUBLE               { $$ = $1; }
    ;

%%

int main(void) {
    printf("Enter expressions, press Ctrl+D to exit\n");
    yyparse();
    return 0;
} 