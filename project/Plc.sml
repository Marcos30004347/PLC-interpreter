(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 2028;
Control.Print.printDepth  := 2028;
Control.Print.stringDepth := 2028;

open PlcFrontEnd;

fun run exp =
  let
    val expType = let in
      teval exp []
    end
    handle UnkownSymbol => let val p = print ("ERR: symbol not found.") in raise UnkownSymbol end
          | IfCondNotBool => let val p =  print ("ERR: 'If' must receive a boolean expression.") in raise IfCondNotBool end
          | DiffBrTypes => let val p =  print ("ERR: 'If' branches must have equal types.") in raise DiffBrTypes end
          | NotEqTypes => let val p =  print ("ERR: comparison must have equal types") in raise NotEqTypes end
          | NoMatchResults => let val p =  print ("ERR: no match results.") in raise NoMatchResults end
          | MatchCondTypesDiff => let val p =  print ("ERR: match condition must have equal types.") in raise MatchCondTypesDiff end
          | MatchResTypeDiff => let val p =  print ("ERR: match response must have equal types.") in raise MatchResTypeDiff end
          | WrongRetType => let val p =  print ("ERR: wrong return type for function.") in raise WrongRetType end
          | CallTypeMisM => let val p =  print ("ERR: argument don't match function argument type.") in raise CallTypeMisM end
          | NotFunc => let val p =  print ("ERR: not a function.") in raise NotFunc end
          | OpNonList => let val p =  print ("ERR: not a list.") in raise OpNonList end
          | ListOutOfRange => let val p =  print ("ERR: list out of range.") in raise ListOutOfRange end
          | EmptySeq => let val p =  print ("ERR: empty sequence") in raise EmptySeq end
          | UnknownType => let val p = print ("ERR: internal error.") in raise UnknownType end
          | _ => let val p = print ("ERR: internal error.") in raise UnknownType end
    val expResult = let in
      eval exp []
    end
    handle UnkownSymbol => let val p = print ("ERR: symbol not found.") in raise UnkownSymbol end
         | InvalidHDOp => let val p =  print ("ERR: can't access head of empty sequence.") in raise InvalidHDOp end
         | InvalidTLOp => let val p =  print ("ERR: can't access tail of empty sequence.") in raise InvalidTLOp end
         | NotFound => let val p =  print ("ERR: no match results.") in raise NotFound end
         | NotAFunc => let val p =  print ("ERR: not a function.") in raise NotAFunc end
         | UnknownImp => let val p = print ("ERR: unknown error.") in raise UnknownImp end
         | _ => let val p = print ("ERR: unknown error.") in raise UnknownImp end
  in
    val2string(expResult) ^ " : " ^ type2string(expType)
  end