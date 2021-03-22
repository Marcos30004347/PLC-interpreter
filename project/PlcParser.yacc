datatype declaration = Variable       of string * expr
                  | Function          of string * expr
                  | RecursiveFunction of string * (plcType * string) list * plcType * expr

fun resolve (decl, prog) =
    case decl of  Variable v           => Let (#1v, #2v, prog)
                | Function f           => Let (#1f, #2f, prog)
                | RecursiveFunction fr => makeFun (#1fr, #2fr, #3fr, #4fr, prog)

fun parserLog(msg) = TextIO.output(TextIO.stdOut, msg ^ "\n")



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
    | SARROW
    | DARROW
    | LESS
    | LBRA
    | RBRA
    | AND
    | LESSEQ
    | NOT
    | DIFF
    | PIPE

%right SEMI SARROW
%left ELSE
%left AND
%left EQUAL DIFF
%left LESS LESSEQ
%right DTWO_POINTS
%left PLUS SUB
%left TIMES DIV

%nonassoc NOT HD TL ISE PRINT ID
%left LSQBRA

%nonterm prog 					of expr
        | statement     of expr
        | exp 					of expr
        | atomic_expr 	of expr
				| const_exp 		of expr
        | app_exp       of expr
				| matchexp 		  of (expr option * expr) list
				| cond_exp 		  of expr option
				| decl 					of declaration
        | atomic_type   of plcType
        | typed_var     of plcType * string
        | plctype       of plcType
        | plctypes      of plcType list
        | args          of (plcType * string) list
        | params        of (plcType * string) list
        | comps         of expr list
    
%prefer PLUS TIMES DIV SUB

%noshift EOF
%nodefault
%verbose

%start prog

%%

prog 				: statement                                           (statement)

statement   : exp                                                 (exp)
            | decl SEMI	statement                  	              (resolve (decl, statement))

decl        : VAR ID EQUAL exp                                    (Variable(ID, exp))
            | FUN ID args EQUAL exp                               (Function(ID, makeAnon(args, exp)))
            | FUN REC ID args TWO_POINTS plctype EQUAL exp        (RecursiveFunction(ID, args, plctype, exp))

exp 				: atomic_expr                                         (atomic_expr)
            | app_exp                                             (app_exp)
						| IF exp THEN exp ELSE exp                            (If(exp1, exp2, exp3))
						| MATCH exp WITCH matchexp                            (Match(exp, matchexp))
						| NOT exp                                             (Prim1("!", exp))
						| SUB exp                                             (Prim1("-", exp))
						| HD exp                                              (Prim1("hd", exp))
						| TL exp                                              (Prim1("tl", exp))
						| ISE exp                                             (Prim1("ise", exp))
            | PRINT exp                                           (Prim1("print", exp))
            | exp AND exp                                         (Prim2("&&", exp1, exp2))
						| exp PLUS exp                                        (Prim2("+", exp1, exp2))
						| exp SUB exp    													            (Prim2("-", exp1, exp2))
						| exp TIMES exp  													            (Prim2("*", exp1, exp2))
						| exp DIV exp    													            (Prim2("/", exp1, exp2))
						| exp EQUAL exp    													          (Prim2("=", exp1, exp2))
						| exp DIFF exp    													          (Prim2("!=", exp1, exp2))
						| exp LESSEQ exp    													        (Prim2("<=", exp1, exp2))
						| exp LESS exp    													          (Prim2("<", exp1, exp2))
						| exp DTWO_POINTS exp    													    (Prim2("::", exp1, exp2))
            | exp SEMI exp                                        (Prim2(";", exp1, exp2))
            | exp LSQBRA NUM RSQBRA                               (Item(NUM, exp))

atomic_expr : const_exp           										            (const_exp)
            | ID       																            (Var(ID))
            | LBRA statement RBRA                                 (statement)
            | LPARENT exp RPARENT 										            (exp)
            | LPARENT comps RPARENT                               (List comps)
            | FN args DARROW exp END                              (makeAnon(args, exp))

app_exp     : atomic_expr atomic_expr                             (Call(atomic_expr1, atomic_expr2))
            | app_exp atomic_expr                                 (Call(atomic_expr, atomic_expr))

const_exp 	: NUM       															            (ConI(NUM))
          	| TRUE      															            (ConB(true))
          	| FALSE     															            (ConB(false))
            | LPARENT RPARENT                                     (List [])
            | LPARENT plctype LSQBRA RSQBRA RPARENT               (ESeq plctype)

comps       : exp COMMA exp                                       ([exp1, exp2])
            | exp COMMA comps                                     ([exp] @ comps)
          
matchexp    : END                                                 ([])
            | PIPE cond_exp SARROW exp matchexp                   ([(cond_exp, exp)] @ matchexp)

cond_exp    : exp                                                 (SOME exp)
            | UNDERSCORE                                          (NONE)

args        : LPARENT RPARENT                                     ([])
            | LPARENT params RPARENT                              (params)

params      : typed_var                                           ([typed_var]) 
            | typed_var COMMA params                              ([typed_var] @ params)

typed_var   : plctype ID                                          ((plctype, ID))

plctype     : atomic_type                                         (atomic_type)
            | LPARENT plctypes RPARENT                            (ListT(plctypes))
            | plctype SARROW plctype                              (FunT(plctype1, plctype2))
            | LSQBRA plctype RSQBRA                               (SeqT(plctype))

atomic_type	:	NIL  																		            (ListT [])
						| INT																			            (IntT)
						| BOOL																		            (BoolT)

plctypes    : plctype COMMA plctype                               ([plctype1, plctype2])
            | plctype COMMA plctypes                              ([plctype1] @ plctypes)

