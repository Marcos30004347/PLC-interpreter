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
    | SEMI 
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
    | END
%left PLUS SUB
%left TIMES DIV

%nonterm prog 					of expr
        | exp 					of expr
        | atomic_expr 	of expr
				| const_exp 		of expr
				| decl 					of expr
				| statement			of expr
        | args          of expr 
        | atomic_type   of plcType

%prefer PLUS TIMES DIV SUB

%noshift EOF
%nodefault
%verbose

%start prog

%%

prog 				: statement(statement)

statement 	: exp											 								(exp) 
						| decl	                   	              (decl)

decl 				: VAR ID EQUAL exp SEMI decl				                  (Let(ID, exp, decl))
     				| FUN ID LPARENT type ID RPARENT EQUAL exp SEMI decl	(Let(ID1, Anon(type, ID2, exp), decl))
     				| FUN ID LPARENT RPARENT EQUAL exp SEMI decl				  (Let(ID1, Anon(ListT([]), "Nil", exp), decl))

args        : LPARENT RPARENT                         (List([]))

exp 				: atomic_expr    													(atomic_expr)
						| exp PLUS exp   													(Prim2("+", exp1, exp2))
						| exp SUB exp    													(Prim2("-", exp1, exp2))
						| exp DIV exp    													(Prim2("/", exp1, exp2))
						| exp TIMES exp  													(Prim2("*", exp1, exp2))

const_exp 	: NUM       															(ConI(NUM))
          	| TRUE      															(ConB(true))
          	| FALSE     															(ConB(false))
					 	| ID       																(Var(ID))


atomic_expr : const_exp           										(const_exp)
            | LPARENT exp RPARENT 										(exp)



atomic_type	:	NIL  																		(ListT([]))
						| INT																			(IntT)
						| BOOL																		(BoolT)
