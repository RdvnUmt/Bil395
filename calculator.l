%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h"
%}

%%

[0-9]+\.[0-9]+  { yylval.doublevalue = atof(yytext); return DOUBLE; }
[0-9]+          { yylval.doublevalue = atof(yytext); return DOUBLE; }
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return TIMES; }
"/"             { return DIVIDE; }
"^"             { return POWER; }
"**"            { return POWER; }
"("             { return LPAREN; }
")"             { return RPAREN; }
\n              { return NEWLINE; }
[ \t]           ;
.               { printf("Invalid expression\n"); }

%%

int yywrap(void) {
    return 1;
} 