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
    | DTWO_POINTS
		| COMMA
    | END
    | GREATER
    | SARROW
    | DARROW
    | LESS
  
%left PLUS SUB
%left TIMES DIV
%right SEMI SARROW DARROW

%nonterm prog 					of expr
        | exp 					of expr
        | atomic_expr 	of expr
				| const_exp 		of expr
				| decl 					of declaration
        | atomic_type   of plcType
        | typed_var     of plcType * string
        | plctype       of plcType
        | plctypes      of plcType list
        | args          of (plcType * string) list
        | params        of (plcType * string) list
    
%prefer PLUS TIMES DIV SUB

%noshift EOF
%nodefault
%verbose

%start prog

%%

prog 				: exp											 								            (exp) 
						| decl SEMI	prog                  	                  (resolve (decl, prog))

decl        : VAR ID EQUAL exp                                    (Variable(ID, exp))
            | FUN ID args EQUAL exp                               (Function(ID, makeAnon(args, exp)))
            | FUN REC ID args TWO_POINTS plctype EQUAL exp        (RecursiveFunction(ID, args, plctype, exp))

exp 				: atomic_expr                                         (atomic_expr)
						| exp PLUS exp                                        (Prim2("+", exp1, exp2))
						| exp SUB exp    													            (Prim2("-", exp1, exp2))
						| exp TIMES exp  													            (Prim2("*", exp1, exp2))
						| exp DIV exp    													            (Prim2("/", exp1, exp2))
          
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
            | LPARENT params RPARENT                              (params)

params      : typed_var                                           ([typed_var]) 
            | typed_var COMMA params                              ([typed_var] @ params)

plctypes    : plctype COMMA plctype                               ([plctype1, plctype2])
            | plctype COMMA plctypes                              ([plctype1] @ plctypes)

plctype     : atomic_type                                         (atomic_type)
            | LPARENT plctypes RPARENT                            (ListT(plctypes))
            | plctype SARROW plctype                              (FunT(plctype1, plctype2))
            | LSQBRA plctype RSQBRA                               (SeqT(plctype))

typed_var   : plctype ID                                          ((plctype, ID))