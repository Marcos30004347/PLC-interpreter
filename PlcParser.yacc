datatype declaration = Variable       of string * expr
                  | Function          of string * expr
                  | RecursiveFunction of string * (plcType * string) list * plcType * expr

fun resolve (decl, prog) =
    case decl of  Variable v           => Let (#1v, #2v, prog)
                | Function f           => Let (#1f, #2f, prog)
                | RecursiveFunction fr => makeFun (#1fr, #2fr, #3fr, #4fr, prog)

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
		| COMMA
    | END
    | GREATER
    | SARROW
    | DARROW
    | LESS
  
%left PLUS SUB
%left TIMES DIV
%right SEMI SARROW

%nonterm prog 					of expr
        | exp 					of expr
        | atomic_expr 	of expr
				| const_exp 		of expr
				| decl 					of expr
				| statement			of declaration
        | atomic_type   of plcType
        | typed_var     of plcType * string
        | types         of plcType list
        | type          of plcType
        | args          of typed_var list
        | params        of plcType list
    
%prefer PLUS TIMES DIV SUB

%noshift EOF
%nodefault
%verbose

%start prog

%%

prog 				: statement                                           (statement)

statement 	: exp											 								            (exp) 
						| decl SEMI	statement                  	              (resolve (decl, statement))

decl        : VAR ID EQUAL exp                                    (Variable(ID, exp))
            | FUN ID args EQUAL exp                               (Function(ID, makeAnon(args, exp)))
            | FUN REC ID args TWO_POINTS type EQUAL exp           (RecursiveFunction(ID, args, type, exp))

exp 				: atomic_expr    													            (atomic_expr)
						| exp PLUS exp   													            (Prim2("+", exp1, exp2))
						| exp SUB exp    													            (Prim2("-", exp1, exp2))
						| exp DIV exp    													            (Prim2("/", exp1, exp2))
						| exp TIMES exp  													            (Prim2("*", exp1, exp2))
          
const_exp 	: NUM       															            (ConI(NUM))
          	| TRUE      															            (ConB(true))
          	| FALSE     															            (ConB(false))
					 	| ID       																            (Var(ID))
          
atomic_expr : const_exp           										            (const_exp)
            | LPARENT exp RPARENT 										            (exp)
            | FN args DARROW exp END                              (makeAnon(args, exp))

atomic_type	:	NIL  																		            (ListT([]))
						| INT																			            (IntT)
						| BOOL																		            (BoolT)

args        : LPARENT RPARENT                                     ([])
            | LPARENT params RPARENT                              (Params)


params      : typed_var                                           ([typed_var]) 
            | typed_var COMMA params                              ([typed_var] @ params)

types       : type COMMA type                                     ([type1, type2])
            | type COMMA types                                    ([type1] @ type2)

type        : LPARENT types RPARENT                               (ListT(types))
            | type SARROW type                                    (FunT(type1, type2))
            | LSQBRA type RSQBRA                                  (SeqT(type))
            | atomic_type                                         (atomic_type)

typed_var   : type ID                                             ((type, ID))