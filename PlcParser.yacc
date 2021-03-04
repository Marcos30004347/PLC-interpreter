%%
%eop EOF
%name PlcParser

%pos int

%term ID of string
    | NUM of int
    | PLUS
    | DIV
    | SUB
    | TIMES
    | EQUAL 
    | TRUE 
    | FALSE 
    | SEMICOLON 
    | EOF 
		| LSQBRA
		| RSQBRA
    | LPARENT
    | RPARENT
    | FUN 
    | VAR 
    | FN 
    | BOOL 
    | INT 
    | ELSE 
    | IF 
    | HD 
    | ISE 
    | MATCH 
    | NIL 
    | PRINT 
    | REC 
    | THEN 
    | TL 
    | WITCH 
    | UNDERSCORE
		| TWO_POINTS
		| ARROW
		| DARROW
		| COMMA

%left PLUS SUB
%left TIMES DIV

%nonterm prog 					of expr
        | exp 					of expr
        | atomic_expr 	of expr
				| const_exp 		of expr
				| decl 					of expr
				| statement			of expr

%prefer PLUS TIMES DIV SUB

%noshift EOF
%nodefault
%verbose

%start prog

%%

prog 				: statement(statement)

statement 	: exp (exp)	
						| decl(decl)

decl 				: VAR ID EQUAL exp SEMICOLON statement 				(Let(ID, exp, statement))

exp 				: atomic_expr    													(atomic_expr)
						| exp PLUS exp   													(Prim2("+", exp1, exp2))
						| exp SUB exp    													(Prim2("-", exp1, exp2))
						| exp DIV exp    													(Prim2("/", exp1, exp2))
						| exp TIMES exp  													(Prim2("*", exp1, exp2))

args 				: LPARENT RPARENT
						| LPARENT params RPARENT	

params			: typed_var																()(*??????*)
						| typed_var COMMA params									()(*??????*)

typed_var		: type ID 																() (*??????*)

type				: atomic_type
						| LPARENT types RPARENT										(types)
						| LSQBRA type RSQBRA											(SeqT(type))
						| type ARROW type													(FunT(type1, type2))

types				: type COMMA type													(type1::type2) (*Probably wrong*)
						| type COMMA types												(type::types) (*Probably wrong*)

atomic_type	:	NIL  																		(ListT()) (*Probably wrong*)
						| INT																			(IntT())
						| BOOL																		(BoolT())

atomic_expr : const_exp           										(const_exp)
            | LPARENT exp RPARENT 										(exp)

const_exp 	: NUM       															(ConI(NUM))
          	| TRUE      															(ConB(true))
          	| FALSE     															(ConB(false))
					 	| ID       																(Var(ID))
