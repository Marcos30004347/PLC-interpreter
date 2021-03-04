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


"("       => (Tokens.LPARENT(!pos,!pos));
")"       => (Tokens.RPARENT(!pos,!pos));
"["       => (Tokens.LSQBRA(!pos,!pos));
"]"       => (Tokens.RSQBRA(!pos,!pos));
";"       => (Tokens.SEMICOLON(!pos,!pos));
","       => (Tokens.COMMA(!pos,!pos));

"+"       => (Tokens.PLUS(!pos,!pos));
"*"       => (Tokens.TIMES(!pos,!pos));
"-"       => (Tokens.SUB(!pos,!pos));
"/"       => (Tokens.DIV(!pos,!pos));
"="       => (Tokens.EQUAL(!pos,!pos));
":"       => (Tokens.TWO_POINTS(!pos,!pos));

"true"    => (lexLog(yypos, yytext); Tokens.TRUE(yypos, yypos+size yytext));
"false"   => (lexLog(yypos, yytext); Tokens.FALSE(yypos, yypos + size yytext));

"=>"      => (Tokens.DARROW(yypos, yypos + size yytext));
"->"      => (Tokens.ARROW(yypos, yypos + size yytext));
"end"     => (Tokens.END(yypos, yypos + size yytext));
"var"     => (Tokens.VAR(yypos, yypos + size yytext));
"fun"     => (Tokens.FUN(yypos, yypos + size yytext));
"fn"      => (Tokens.FN(yypos, yypos + size yytext));
"Bool"    => (Tokens.BOOL(yypos, yypos + size yytext));
"Int"     => (Tokens.INT(yypos, yypos + size yytext));
"else"    => (Tokens.ELSE(yypos, yypos + size yytext));
"if"      => (Tokens.IF(yypos, yypos + size yytext));
"hd"      => (Tokens.HD(yypos, yypos + size yytext));
"ise"     => (Tokens.ISE(yypos, yypos + size yytext));
"match"   => (Tokens.MATCH(yypos, yypos + size yytext));
"Nil"     => (Tokens.NIL(yypos, yypos + size yytext));
"print"   => (Tokens.PRINT(yypos, yypos + size yytext));
"rec"     => (Tokens.REC(yypos, yypos + size yytext));
"then"    => (Tokens.THEN(yypos, yypos + size yytext));
"tl"      => (Tokens.TL(yypos, yypos + size yytext));
"with"    => (Tokens.WITCH(yypos, yypos + size yytext));
"_"       => (Tokens.UNDERSCORE(yypos, yypos + size yytext));

{digit}+  => (Tokens.NUM(revfold (fn (a,r) => ord(a)-ord(#"0")+10*r) (explode yytext) 0, !pos,!pos));
{alpha}+   => (Tokens.ID(yytext, yypos, yypos + size yytext));