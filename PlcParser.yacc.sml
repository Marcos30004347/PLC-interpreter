functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
datatype declaration = Variable of string * expr
                  | Function of string * expr
                  | RecursiveFunction of string * (plcType * string) list * plcType * expr

fun resolve (decl, prog) =
    case decl of  Variable v => Let (#1v, #2v, prog)
                | Function f           => Let (#1f, #2f, prog)
                | RecursiveFunction fr => makeFun (#1fr, #2fr, #3fr, #4fr, prog)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\089\000\007\000\089\000\013\000\089\000\015\000\089\000\
\\034\000\089\000\000\000\
\\001\000\001\000\090\000\007\000\090\000\013\000\090\000\015\000\090\000\
\\034\000\090\000\000\000\
\\001\000\001\000\091\000\007\000\091\000\013\000\091\000\015\000\091\000\
\\034\000\091\000\000\000\
\\001\000\001\000\098\000\007\000\098\000\013\000\098\000\015\000\098\000\
\\034\000\098\000\000\000\
\\001\000\001\000\099\000\007\000\099\000\013\000\099\000\015\000\099\000\
\\034\000\099\000\000\000\
\\001\000\001\000\100\000\007\000\100\000\013\000\100\000\015\000\100\000\
\\034\000\100\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\008\000\013\000\009\000\012\000\
\\014\000\011\000\016\000\010\000\017\000\009\000\018\000\008\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\008\000\013\000\009\000\012\000\
\\014\000\011\000\018\000\008\000\000\000\
\\001\000\001\000\023\000\000\000\
\\001\000\001\000\025\000\028\000\024\000\000\000\
\\001\000\001\000\044\000\000\000\
\\001\000\001\000\049\000\000\000\
\\001\000\003\000\077\000\004\000\077\000\005\000\077\000\006\000\077\000\
\\010\000\077\000\011\000\077\000\015\000\077\000\035\000\077\000\000\000\
\\001\000\003\000\078\000\004\000\019\000\005\000\078\000\006\000\017\000\
\\010\000\078\000\011\000\078\000\015\000\078\000\035\000\078\000\000\000\
\\001\000\003\000\079\000\004\000\019\000\005\000\079\000\006\000\017\000\
\\010\000\079\000\011\000\079\000\015\000\079\000\035\000\079\000\000\000\
\\001\000\003\000\080\000\004\000\080\000\005\000\080\000\006\000\080\000\
\\010\000\080\000\011\000\080\000\015\000\080\000\035\000\080\000\000\000\
\\001\000\003\000\081\000\004\000\081\000\005\000\081\000\006\000\081\000\
\\010\000\081\000\011\000\081\000\015\000\081\000\035\000\081\000\000\000\
\\001\000\003\000\082\000\004\000\082\000\005\000\082\000\006\000\082\000\
\\010\000\082\000\011\000\082\000\015\000\082\000\035\000\082\000\000\000\
\\001\000\003\000\083\000\004\000\083\000\005\000\083\000\006\000\083\000\
\\010\000\083\000\011\000\083\000\015\000\083\000\035\000\083\000\000\000\
\\001\000\003\000\084\000\004\000\084\000\005\000\084\000\006\000\084\000\
\\010\000\084\000\011\000\084\000\015\000\084\000\035\000\084\000\000\000\
\\001\000\003\000\085\000\004\000\085\000\005\000\085\000\006\000\085\000\
\\010\000\085\000\011\000\085\000\015\000\085\000\035\000\085\000\000\000\
\\001\000\003\000\086\000\004\000\086\000\005\000\086\000\006\000\086\000\
\\010\000\086\000\011\000\086\000\015\000\086\000\035\000\086\000\000\000\
\\001\000\003\000\087\000\004\000\087\000\005\000\087\000\006\000\087\000\
\\010\000\087\000\011\000\087\000\015\000\087\000\035\000\087\000\000\000\
\\001\000\003\000\088\000\004\000\088\000\005\000\088\000\006\000\088\000\
\\010\000\088\000\011\000\088\000\015\000\088\000\035\000\088\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\005\000\018\000\006\000\017\000\
\\010\000\074\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\005\000\018\000\006\000\017\000\
\\010\000\075\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\005\000\018\000\006\000\017\000\
\\010\000\076\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\005\000\018\000\006\000\017\000\
\\011\000\072\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\005\000\018\000\006\000\017\000\
\\015\000\046\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\005\000\018\000\006\000\017\000\
\\035\000\064\000\000\000\
\\001\000\007\000\092\000\033\000\092\000\000\000\
\\001\000\007\000\093\000\033\000\093\000\000\000\
\\001\000\007\000\032\000\000\000\
\\001\000\007\000\043\000\000\000\
\\001\000\007\000\056\000\000\000\
\\001\000\007\000\068\000\000\000\
\\001\000\010\000\016\000\000\000\
\\001\000\011\000\000\000\000\000\
\\001\000\011\000\071\000\000\000\
\\001\000\011\000\073\000\000\000\
\\001\000\012\000\042\000\014\000\041\000\015\000\040\000\019\000\039\000\
\\020\000\038\000\026\000\037\000\000\000\
\\001\000\012\000\042\000\014\000\041\000\019\000\039\000\020\000\038\000\
\\026\000\037\000\000\000\
\\001\000\013\000\061\000\000\000\
\\001\000\014\000\022\000\000\000\
\\001\000\015\000\094\000\034\000\050\000\000\000\
\\001\000\015\000\095\000\000\000\
\\001\000\015\000\096\000\034\000\059\000\000\000\
\\001\000\015\000\097\000\000\000\
\\001\000\015\000\101\000\034\000\101\000\000\000\
\\001\000\015\000\048\000\000\000\
\\001\000\015\000\060\000\000\000\
\\001\000\033\000\062\000\000\000\
\\001\000\034\000\059\000\000\000\
\\001\000\036\000\047\000\000\000\
\"
val actionRowNumbers =
"\006\000\038\000\036\000\021\000\
\\012\000\027\000\043\000\008\000\
\\009\000\007\000\019\000\018\000\
\\017\000\020\000\006\000\007\000\
\\007\000\007\000\007\000\032\000\
\\040\000\033\000\010\000\043\000\
\\028\000\039\000\016\000\014\000\
\\015\000\013\000\053\000\049\000\
\\011\000\044\000\005\000\000\000\
\\001\000\002\000\030\000\041\000\
\\041\000\007\000\043\000\034\000\
\\022\000\007\000\031\000\048\000\
\\041\000\052\000\050\000\042\000\
\\024\000\051\000\007\000\029\000\
\\045\000\041\000\003\000\004\000\
\\041\000\025\000\023\000\046\000\
\\047\000\035\000\007\000\026\000\
\\037\000"
val gotoT =
"\
\\001\000\068\000\002\000\005\000\003\000\004\000\004\000\003\000\
\\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\019\000\000\000\
\\000\000\
\\000\000\
\\002\000\024\000\003\000\004\000\004\000\003\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\005\000\003\000\004\000\004\000\003\000\005\000\002\000\
\\006\000\025\000\000\000\
\\002\000\026\000\003\000\004\000\004\000\003\000\000\000\
\\002\000\027\000\003\000\004\000\004\000\003\000\000\000\
\\002\000\028\000\003\000\004\000\004\000\003\000\000\000\
\\002\000\029\000\003\000\004\000\004\000\003\000\000\000\
\\000\000\
\\007\000\034\000\008\000\033\000\010\000\032\000\012\000\031\000\000\000\
\\000\000\
\\000\000\
\\011\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\034\000\009\000\050\000\010\000\049\000\000\000\
\\007\000\034\000\010\000\051\000\000\000\
\\002\000\052\000\003\000\004\000\004\000\003\000\000\000\
\\011\000\053\000\000\000\
\\000\000\
\\000\000\
\\002\000\055\000\003\000\004\000\004\000\003\000\000\000\
\\000\000\
\\000\000\
\\007\000\034\000\008\000\033\000\010\000\032\000\012\000\056\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\061\000\003\000\004\000\004\000\003\000\000\000\
\\000\000\
\\000\000\
\\007\000\034\000\009\000\064\000\010\000\063\000\000\000\
\\000\000\
\\000\000\
\\007\000\034\000\010\000\065\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\067\000\003\000\004\000\004\000\003\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 69
val numrules = 31
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | params of unit ->  (plcType list)
 | args of unit ->  (typed_var list) | type of unit ->  (plcType)
 | types of unit ->  (plcType list)
 | typed_var of unit ->  (plcType*string)
 | atomic_type of unit ->  (plcType)
 | statement of unit ->  (declaration) | decl of unit ->  (expr)
 | const_exp of unit ->  (expr) | atomic_expr of unit ->  (expr)
 | exp of unit ->  (expr) | prog of unit ->  (expr)
end
type svalue = MlyValue.svalue
type result = expr
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 2))::
(nil
,nil
 $$ (T 5))::
(nil
,nil
 $$ (T 3))::
(nil
,nil
 $$ (T 4))::
nil
val noShift = 
fn (T 10) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "PLUS"
  | (T 3) => "DIV"
  | (T 4) => "SUB"
  | (T 5) => "TIMES"
  | (T 6) => "EQUAL"
  | (T 7) => "TRUE"
  | (T 8) => "FALSE"
  | (T 9) => "SEMI"
  | (T 10) => "EOF"
  | (T 11) => "LSQBRA"
  | (T 12) => "RSQBRA"
  | (T 13) => "LPARENT"
  | (T 14) => "RPARENT"
  | (T 15) => "FUN"
  | (T 16) => "VAR"
  | (T 17) => "FN"
  | (T 18) => "BOOL"
  | (T 19) => "INT"
  | (T 20) => "ELSE"
  | (T 21) => "IF"
  | (T 22) => "HD"
  | (T 23) => "ISE"
  | (T 24) => "MATCH"
  | (T 25) => "NIL"
  | (T 26) => "PRINT"
  | (T 27) => "REC"
  | (T 28) => "THEN"
  | (T 29) => "TL"
  | (T 30) => "WITCH"
  | (T 31) => "UNDERSCORE"
  | (T 32) => "TWO_POINTS"
  | (T 33) => "COMMA"
  | (T 34) => "END"
  | (T 35) => "GREATER"
  | (T 36) => "LESS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.prog (fn
 _ => let val  (statement as statement1) = statement1 ()
 in (statement)
end)
 in ( LrTable.NT 0, ( result, statement1left, statement1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.statement (fn _ => let val  (exp as 
exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 5, ( result, exp1left, exp1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.statement statement1, _, statement1right))
 :: _ :: ( _, ( MlyValue.decl decl1, decl1left, _)) :: rest671)) =>
 let val  result = MlyValue.statement (fn _ => let val  (decl as decl1
) = decl1 ()
 val  (statement as statement1) = statement1 ()
 in (resolve (decl, statement))
end)
 in ( LrTable.NT 5, ( result, decl1left, statement1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.decl (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (Variable(ID, exp))
end)
 in ( LrTable.NT 4, ( result, VAR1left, exp1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.args args1, _, _)) :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, 
( _, FUN1left, _)) :: rest671)) => let val  result = MlyValue.decl (fn
 _ => let val  (ID as ID1) = ID1 ()
 val  (args as args1) = args1 ()
 val  (exp as exp1) = exp1 ()
 in (Function(ID, makeAnon(args, exp)))
end)
 in ( LrTable.NT 4, ( result, FUN1left, exp1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.type type1, _, _)) :: _ :: ( _, ( MlyValue.args args1, _, _))
 :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, FUN1left, _)) :: 
rest671)) => let val  result = MlyValue.decl (fn _ => let val  (ID as 
ID1) = ID1 ()
 val  (args as args1) = args1 ()
 val  (type as type1) = type1 ()
 val  (exp as exp1) = exp1 ()
 in (RecursiveFunction(ID, args, type, exp))
end)
 in ( LrTable.NT 4, ( result, FUN1left, exp1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.atomic_expr atomic_expr1, atomic_expr1left, 
atomic_expr1right)) :: rest671)) => let val  result = MlyValue.exp (fn
 _ => let val  (atomic_expr as atomic_expr1) = atomic_expr1 ()
 in (atomic_expr)
end)
 in ( LrTable.NT 1, ( result, atomic_expr1left, atomic_expr1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Prim2("+", exp1, exp2))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Prim2("-", exp1, exp2))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Prim2("/", exp1, exp2))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Prim2("*", exp1, exp2))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.const_exp (fn _ => let val  (NUM as 
NUM1) = NUM1 ()
 in (ConI(NUM))
end)
 in ( LrTable.NT 3, ( result, NUM1left, NUM1right), rest671)
end
|  ( 12, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.const_exp (fn _ => (ConB(true)))
 in ( LrTable.NT 3, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 13, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.const_exp (fn _ => (ConB(false)))
 in ( LrTable.NT 3, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.const_exp (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (Var(ID))
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.const_exp const_exp1, const_exp1left, 
const_exp1right)) :: rest671)) => let val  result = 
MlyValue.atomic_expr (fn _ => let val  (const_exp as const_exp1) = 
const_exp1 ()
 in (const_exp)
end)
 in ( LrTable.NT 2, ( result, const_exp1left, const_exp1right), 
rest671)
end
|  ( 16, ( ( _, ( _, _, RPARENT1right)) :: ( _, ( MlyValue.exp exp1, _
, _)) :: ( _, ( _, LPARENT1left, _)) :: rest671)) => let val  result =
 MlyValue.atomic_expr (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 2, ( result, LPARENT1left, RPARENT1right), rest671)

end
|  ( 17, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exp exp1, _, _)
) :: _ :: _ :: ( _, ( MlyValue.args args1, _, _)) :: ( _, ( _, FN1left
, _)) :: rest671)) => let val  result = MlyValue.atomic_expr (fn _ =>
 let val  (args as args1) = args1 ()
 val  (exp as exp1) = exp1 ()
 in (makeAnon(args, exp))
end)
 in ( LrTable.NT 2, ( result, FN1left, END1right), rest671)
end
|  ( 18, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.atomic_type (fn _ => (ListT([])))
 in ( LrTable.NT 6, ( result, NIL1left, NIL1right), rest671)
end
|  ( 19, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.atomic_type (fn _ => (IntT))
 in ( LrTable.NT 6, ( result, INT1left, INT1right), rest671)
end
|  ( 20, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.atomic_type (fn _ => (BoolT))
 in ( LrTable.NT 6, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RPARENT1right)) :: ( _, ( _, LPARENT1left, _))
 :: rest671)) => let val  result = MlyValue.args (fn _ => ([]))
 in ( LrTable.NT 10, ( result, LPARENT1left, RPARENT1right), rest671)

end
|  ( 22, ( ( _, ( _, _, RPARENT1right)) :: ( _, ( MlyValue.params 
params1, _, _)) :: ( _, ( _, LPARENT1left, _)) :: rest671)) => let
 val  result = MlyValue.args (fn _ => let val  params1 = params1 ()
 in (Params)
end)
 in ( LrTable.NT 10, ( result, LPARENT1left, RPARENT1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.typed_var typed_var1, typed_var1left, 
typed_var1right)) :: rest671)) => let val  result = MlyValue.params
 (fn _ => let val  (typed_var as typed_var1) = typed_var1 ()
 in ([typed_var])
end)
 in ( LrTable.NT 11, ( result, typed_var1left, typed_var1right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.params params1, _, params1right)) :: _ :: (
 _, ( MlyValue.typed_var typed_var1, typed_var1left, _)) :: rest671))
 => let val  result = MlyValue.params (fn _ => let val  (typed_var as 
typed_var1) = typed_var1 ()
 val  (params as params1) = params1 ()
 in ([typed_var] @ params)
end)
 in ( LrTable.NT 11, ( result, typed_var1left, params1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.type type2, _, type2right)) :: _ :: ( _, ( 
MlyValue.type type1, type1left, _)) :: rest671)) => let val  result = 
MlyValue.types (fn _ => let val  type1 = type1 ()
 val  type2 = type2 ()
 in ([type1, type2])
end)
 in ( LrTable.NT 8, ( result, type1left, type2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.types types1, _, types1right)) :: _ :: ( _,
 ( MlyValue.type type1, type1left, _)) :: rest671)) => let val  result
 = MlyValue.types (fn _ => let val  type1 = type1 ()
 val  types1 = types1 ()
 in ([type1] @ type2)
end)
 in ( LrTable.NT 8, ( result, type1left, types1right), rest671)
end
|  ( 27, ( ( _, ( _, _, RPARENT1right)) :: ( _, ( MlyValue.types 
types1, _, _)) :: ( _, ( _, LPARENT1left, _)) :: rest671)) => let val 
 result = MlyValue.type (fn _ => let val  (types as types1) = types1
 ()
 in (ListT(types))
end)
 in ( LrTable.NT 9, ( result, LPARENT1left, RPARENT1right), rest671)

end
|  ( 28, ( ( _, ( _, _, RSQBRA1right)) :: ( _, ( MlyValue.type type1,
 _, _)) :: ( _, ( _, LSQBRA1left, _)) :: rest671)) => let val  result
 = MlyValue.type (fn _ => let val  (type as type1) = type1 ()
 in (SeqT(type))
end)
 in ( LrTable.NT 9, ( result, LSQBRA1left, RSQBRA1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.atomic_type atomic_type1, atomic_type1left,
 atomic_type1right)) :: rest671)) => let val  result = MlyValue.type
 (fn _ => let val  (atomic_type as atomic_type1) = atomic_type1 ()
 in (atomic_type)
end)
 in ( LrTable.NT 9, ( result, atomic_type1left, atomic_type1right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( 
MlyValue.type type1, type1left, _)) :: rest671)) => let val  result = 
MlyValue.typed_var (fn _ => let val  (type as type1) = type1 ()
 val  (ID as ID1) = ID1 ()
 in ((type, ID))
end)
 in ( LrTable.NT 7, ( result, type1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LSQBRA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RSQBRA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LPARENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RPARENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun HD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun ISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun REC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun TL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun WITCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDERSCORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun TWO_POINTS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
end
end
