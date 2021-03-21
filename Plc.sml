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
          | InvalidCondition => let val p =  print ("ERR: 'If' must receive a boolean expression.") in raise InvalidCondition end
          | InavalidBranches => let val p =  print ("ERR: 'If' branches must have equal types.") in raise InavalidBranches end
          | TypeMismatch => let val p =  print ("ERR: comparison must have equal types") in raise TypeMismatch end
          | NoMatchFound => let val p =  print ("ERR: no match results.") in raise NoMatchFound end
          | CondTypeMismatch => let val p =  print ("ERR: match condition must have equal types.") in raise CondTypeMismatch end
          | ResTypeMismatch => let val p =  print ("ERR: match response must have equal types.") in raise ResTypeMismatch end
          | InvalidReturn => let val p =  print ("ERR: wrong return type for function.") in raise InvalidReturn end
          | InvalidArg => let val p =  print ("ERR: argument don't match function argument type.") in raise InvalidArg end
          | InvalidFunc => let val p =  print ("ERR: not a function.") in raise InvalidFunc end
          | NotList => let val p =  print ("ERR: not a list.") in raise NotList end
          | OutOfRange => let val p =  print ("ERR: list out of range.") in raise OutOfRange end
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