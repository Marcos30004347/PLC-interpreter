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

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\012\000\002\000\011\000\008\000\010\000\009\000\009\000\
\\012\000\008\000\000\000\
\\001\000\001\000\012\000\002\000\011\000\008\000\010\000\009\000\009\000\
\\012\000\008\000\015\000\007\000\000\000\
\\001\000\003\000\030\000\004\000\030\000\005\000\030\000\006\000\030\000\
\\007\000\030\000\011\000\030\000\013\000\030\000\000\000\
\\001\000\003\000\031\000\004\000\015\000\005\000\031\000\006\000\013\000\
\\007\000\031\000\011\000\031\000\013\000\031\000\000\000\
\\001\000\003\000\032\000\004\000\015\000\005\000\032\000\006\000\013\000\
\\007\000\032\000\011\000\032\000\013\000\032\000\000\000\
\\001\000\003\000\033\000\004\000\033\000\005\000\033\000\006\000\033\000\
\\007\000\033\000\011\000\033\000\013\000\033\000\000\000\
\\001\000\003\000\034\000\004\000\034\000\005\000\034\000\006\000\034\000\
\\007\000\034\000\011\000\034\000\013\000\034\000\000\000\
\\001\000\003\000\035\000\004\000\035\000\005\000\035\000\006\000\035\000\
\\007\000\035\000\011\000\035\000\013\000\035\000\000\000\
\\001\000\003\000\036\000\004\000\036\000\005\000\036\000\006\000\036\000\
\\007\000\036\000\011\000\036\000\013\000\036\000\000\000\
\\001\000\003\000\037\000\004\000\037\000\005\000\037\000\006\000\037\000\
\\007\000\037\000\011\000\037\000\013\000\037\000\000\000\
\\001\000\003\000\038\000\004\000\038\000\005\000\038\000\006\000\038\000\
\\007\000\038\000\011\000\038\000\013\000\038\000\000\000\
\\001\000\003\000\039\000\004\000\039\000\005\000\039\000\006\000\039\000\
\\007\000\039\000\011\000\039\000\013\000\039\000\000\000\
\\001\000\003\000\040\000\004\000\040\000\005\000\040\000\006\000\040\000\
\\007\000\040\000\011\000\040\000\013\000\040\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\005\000\014\000\006\000\013\000\
\\007\000\023\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\005\000\014\000\006\000\013\000\
\\011\000\027\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\005\000\014\000\006\000\013\000\
\\011\000\029\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\005\000\014\000\006\000\013\000\
\\013\000\024\000\000\000\
\\001\000\011\000\000\000\000\000\
\\001\000\011\000\028\000\000\000\
\"
val actionRowNumbers =
"\001\000\018\000\007\000\002\000\
\\014\000\000\000\000\000\011\000\
\\010\000\009\000\012\000\000\000\
\\000\000\000\000\000\000\013\000\
\\016\000\006\000\004\000\005\000\
\\003\000\000\000\008\000\015\000\
\\017\000"
val gotoT =
"\
\\001\000\024\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\015\000\003\000\003\000\004\000\002\000\000\000\
\\002\000\016\000\003\000\003\000\004\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\017\000\003\000\003\000\004\000\002\000\000\000\
\\002\000\018\000\003\000\003\000\004\000\002\000\000\000\
\\002\000\019\000\003\000\003\000\004\000\002\000\000\000\
\\002\000\020\000\003\000\003\000\004\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\023\000\003\000\003\000\004\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 25
val numrules = 14
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
 | decl of unit ->  (expr) | const_exp of unit ->  (expr)
 | atomic_expr of unit ->  (expr) | exp of unit ->  (expr)
 | prog of unit ->  (expr)
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
  | (T 9) => "SEMICOLON"
  | (T 10) => "EOF"
  | (T 11) => "LPARENT"
  | (T 12) => "RPARENT"
  | (T 13) => "FUN"
  | (T 14) => "VAR"
  | (T 15) => "FN"
  | (T 16) => "BOOL"
  | (T 17) => "INT"
  | (T 18) => "ELSE"
  | (T 19) => "IF"
  | (T 20) => "HD"
  | (T 21) => "ISE"
  | (T 22) => "MATCH"
  | (T 23) => "NIL"
  | (T 24) => "PRINT"
  | (T 25) => "REC"
  | (T 26) => "THEN"
  | (T 27) => "TL"
  | (T 28) => "WITCH"
  | (T 29) => "UNDERSCORE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
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
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.prog (fn _ => let val  (exp as exp1) =
 exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.decl decl1, decl1left, decl1right)) :: 
rest671)) => let val  result = MlyValue.prog (fn _ => let val  (decl
 as decl1) = decl1 ()
 in (decl)
end)
 in ( LrTable.NT 0, ( result, decl1left, decl1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) =>
 let val  result = MlyValue.decl (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Let("=", exp1, exp2))
end)
 in ( LrTable.NT 4, ( result, VAR1left, exp2right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.atomic_expr atomic_expr1, atomic_expr1left, 
atomic_expr1right)) :: rest671)) => let val  result = MlyValue.exp (fn
 _ => let val  (atomic_expr as atomic_expr1) = atomic_expr1 ()
 in (atomic_expr)
end)
 in ( LrTable.NT 1, ( result, atomic_expr1left, atomic_expr1right), 
rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Prim2("+", exp1, exp2))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Prim2("-", exp1, exp2))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Prim2("/", exp1, exp2))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Prim2("*", exp1, exp2))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.const_exp const_exp1, const_exp1left, 
const_exp1right)) :: rest671)) => let val  result = 
MlyValue.atomic_expr (fn _ => let val  (const_exp as const_exp1) = 
const_exp1 ()
 in (const_exp)
end)
 in ( LrTable.NT 2, ( result, const_exp1left, const_exp1right), 
rest671)
end
|  ( 9, ( ( _, ( _, _, RPARENT1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPARENT1left, _)) :: rest671)) => let val  result = 
MlyValue.atomic_expr (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 2, ( result, LPARENT1left, RPARENT1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.const_exp (fn _ => let val  (NUM as 
NUM1) = NUM1 ()
 in (ConI(NUM))
end)
 in ( LrTable.NT 3, ( result, NUM1left, NUM1right), rest671)
end
|  ( 11, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.const_exp (fn _ => (ConB(true)))
 in ( LrTable.NT 3, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 12, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.const_exp (fn _ => (ConB(false)))
 in ( LrTable.NT 3, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.const_exp (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (Var(ID))
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
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
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LPARENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RPARENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun HD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun ISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun REC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun TL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun WITCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDERSCORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
end
end
