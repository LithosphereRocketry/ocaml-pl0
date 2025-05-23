%{
open Ast
%}
%token <int> NUMBER
%token <string> IDENT
// Keywords
%token PROCEDURE CONST VAR CALL QUERY DISPLAY BEGIN END IF THEN WHILE DO ODD
// Operators
%token UPLUS UMINUS PLUS MINUS TIMES DIVIDE
%token EQUALS NOTEQUALS LESS GREATER LESSEQUAL GREATEREQUAL
// Structural symbols
%token ASSIGN COMMA ENDSTMT ENDPROG

// Operator associativity
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS UPLUS

// Entry point
%start program
%type <block> program
%%
program:
        block ENDPROG { $1 }
block:
        block_var { $1 }
    |   CONST constblock ENDSTMT block_var { { $4 with constdef = $2 } }
block_var:
        block_proc { $1 }
    |   VAR varblock ENDSTMT block_proc { { $4 with vardef = $2 } }
block_proc:
        ENDSTMT { { constdef = []; vardef = []; procdef = [] ; stmt = Empty } }
    |   statement ENDSTMT { { constdef = []; vardef = []; procdef = [] ; stmt = $1 } }
    |   procedure block_proc { { $2 with procdef = $1 :: $2.procdef }}
constblock:
        IDENT EQUALS NUMBER { ($1, $3) :: [] }
    |   IDENT EQUALS NUMBER COMMA constblock { ($1, $3) :: $5 }
varblock:
        IDENT { $1 :: [] }
    |   IDENT COMMA varblock { $1 :: $3 }
statement:
        IDENT ASSIGN expression { Assignment ($1, $3) }
    |   CALL IDENT { Call $2 }
    |   QUERY IDENT { Query $2 }
    |   DISPLAY IDENT { Display $2 }
    |   BEGIN statement beginblock { Begin $2 :: $3 }
    |   IF condition THEN statement { If ($2, $4) }
    |   WHILE condition DO statement { While ($2, $4) }
beginblock:
        END { [] }
    |   ENDSTMT statement beginblock { $2 :: $3 }
procedure:
        PROCEDURE IDENT ENDSTMT block ENDSTMT { { name = $2; body = $4 }}
expression:
        PLUS expression %prec UPLUS { Prefix (Plus, $2) }
    |   MINUS expression %prec UMINUS { Prefix (Minus, $2) }
    |   expression PLUS expression { Infix ($1, Plus, $3) }
    |   expression MINUS expression { Infix ($1, Minus, $3) }
    |   expression TIMES expression { Infix ($1, Times, $3) }
    |   expression DIVIDE expression { Infix ($1, Divide, $3) }
    |   NUMBER { Literal $1 }
    |   IDENT { Variable $1 }
condition:
        ODD expression { Odd $2 }
    |   expression EQUALS expression { Comparison ($1, Equals, $3) }
    |   expression NOTEQUALS expression { Comparison ($1, NotEquals, $3) }
    |   expression LESS expression { Comparison ($1, LessThan, $3) }
    |   expression LESSEQUAL expression { Comparison ($1, LessEqual, $3) }
    |   expression GREATER expression { Comparison ($1, GreaterThan, $3) }
    |   expression GREATEREQUAL expression { Comparison ($1, GreaterEqual, $3) }
%%