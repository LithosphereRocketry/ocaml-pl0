%{

%}
%token <int> INT
// Keywords
%token CONST
// Operators
%token EQUALS
// Structural symbols
%token ENDSTMT ENDPROG
%start program
%type <block> program
%%
program:
    block ENDPROG { $1 }
;
block:
        CONST EQUALS ENDSTMT   { block() }
;
%%