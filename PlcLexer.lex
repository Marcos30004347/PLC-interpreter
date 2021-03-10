(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0


val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0


fun revfold _ nil b = b 
  | revfold f (hd::tl) b = revfold f tl (f(hd,b));

fun lexLog(pos, msg) = TextIO.output(TextIO.stdOut, msg ^ "\n")


(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()


%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%

{ws}+     => (lex());
\n        => (pos := (!pos) + 1; lex());

"->"      => (lexLog(yypos, yytext); Tokens.SARROW(yypos, yypos + size yytext));
"=>"      => (lexLog(yypos, yytext); Tokens.DARROW(yypos, yypos + size yytext));
"end"     => (lexLog(yypos, yytext); Tokens.END(yypos, yypos + size yytext));
"var"     => (lexLog(yypos, yytext); Tokens.VAR(yypos, yypos + size yytext));
"fun"     => (lexLog(yypos, yytext); Tokens.FUN(yypos, yypos + size yytext));
"fn"      => (lexLog(yypos, yytext); Tokens.FN(yypos, yypos + size yytext));
"Bool"    => (lexLog(yypos, yytext); Tokens.BOOL(yypos, yypos + size yytext));
"Int"     => (lexLog(yypos, yytext); Tokens.INT(yypos, yypos + size yytext));
"Nil"     => (lexLog(yypos, yytext); Tokens.NIL(yypos, yypos + size yytext));
"else"    => (lexLog(yypos, yytext); Tokens.ELSE(yypos, yypos + size yytext));
"if"      => (lexLog(yypos, yytext); Tokens.IF(yypos, yypos + size yytext));
"hd"      => (lexLog(yypos, yytext); Tokens.HD(yypos, yypos + size yytext));
"ise"     => (lexLog(yypos, yytext); Tokens.ISE(yypos, yypos + size yytext));
"match"   => (lexLog(yypos, yytext); Tokens.MATCH(yypos, yypos + size yytext));
"print"   => (lexLog(yypos, yytext); Tokens.PRINT(yypos, yypos + size yytext));
"rec"     => (lexLog(yypos, yytext); Tokens.REC(yypos, yypos + size yytext));
"then"    => (lexLog(yypos, yytext); Tokens.THEN(yypos, yypos + size yytext));
"tl"      => (lexLog(yypos, yytext); Tokens.TL(yypos, yypos + size yytext));
"with"    => (lexLog(yypos, yytext); Tokens.WITCH(yypos, yypos + size yytext));
"_"       => (lexLog(yypos, yytext); Tokens.UNDERSCORE(yypos, yypos + size yytext));

"("       => (Tokens.LPARENT(!pos,!pos));
")"       => (Tokens.RPARENT(!pos,!pos));
"["       => (Tokens.LSQBRA(!pos,!pos));
"]"       => (Tokens.RSQBRA(!pos,!pos));
";"       => (Tokens.SEMI(!pos,!pos));
","       => (Tokens.COMMA(!pos,!pos));

"+"       => (Tokens.PLUS(!pos,!pos));
"*"       => (Tokens.TIMES(!pos,!pos));
"-"       => (Tokens.SUB(!pos,!pos));
"/"       => (Tokens.DIV(!pos,!pos));
"="       => (Tokens.EQUAL(!pos,!pos));
">"       => (Tokens.GREATER(!pos,!pos));
"<"       => (Tokens.LESS(!pos,!pos));
":"       => (Tokens.TWO_POINTS(!pos,!pos));

"true"    => (lexLog(yypos, yytext); Tokens.TRUE(yypos, yypos+size yytext));
"false"   => (lexLog(yypos, yytext); Tokens.FALSE(yypos, yypos + size yytext));


{digit}+  => (lexLog(yypos, yytext); Tokens.NUM(revfold (fn (a,r) => ord(a)-ord(#"0")+10*r) (explode yytext) 0, !pos,!pos));
{alpha}+   => (lexLog(yypos, yytext); Tokens.ID(yytext, yypos, yypos + size yytext));