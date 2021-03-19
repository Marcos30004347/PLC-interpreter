(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (ConI i) _ = IntV i
  | eval (ConB b) _ = BoolV b
  | eval (ESeq e) _ = SeqV []
  | eval (Var v) (env:plcVal env) = let in lookup env v handle SymbolNotFound => raise SymbolNotFound end
  | eval (Prim2 (opr, e1, e2)) (env:plcVal env) =
    if opr = ";" then
      let
        val ignore = eval e1 env
      in
        eval e2 env
      end
    else
      let
        val v1 = eval e1 env
        val v2 = eval e2 env
      in
        case (v1, v2) of (IntV i1, IntV i2) => 
            let in
              case opr of "+" => IntV (i1 + i2)
                | "-" => IntV (i1 - i2)
                | "*" => IntV (i1 * i2)
                | "/" => IntV (i1 div i2)
                | "<" => BoolV (i1 < i2)
                | "<=" => BoolV (i1 <= i2)
                | "=" => BoolV (i1 = i2)
                | "!=" => BoolV (i1 <> i2)
                | _ => raise Impossible
            end
          | (BoolV b1, BoolV b2) => 
            let in
              case opr of "&&" => BoolV (b1 andalso b2)
                | "=" => BoolV (b1 = b2)
                | "!=" => BoolV (b1 <> b2)
                | _ => raise Impossible
            end
          | (IntV i1, SeqV s2) => 
            let in
              case opr of "::" => SeqV (IntV i1 :: s2)
                | _ => raise Impossible
            end
          | (BoolV b1, SeqV s2) => 
            let in
              case opr of "::" => SeqV (BoolV b1 :: s2)
                | _ => raise Impossible
            end
          | (ListV l1, SeqV s2) => 
            let in
              case opr of "::" => SeqV (ListV l1 :: s2)
                | _ => raise Impossible
            end
          | _ => raise Impossible
      end
  | eval (Prim1 (opr, exp)) (env:plcVal env) =
    let
      val v = eval exp env
    in
      case v of
          IntV i => 
          let in
            case opr of "print" => 
                let 
                  val v = IntV i
                  val ignore = print(val2string(v) ^ "\n")
                in
                  ListV []
                end
              |  "-" => IntV (~ i)
              | _ => raise Impossible
          end
        | BoolV b =>
          let in
            case opr of "print" => 
                let 
                  val v = BoolV b
                  val ignore = print(val2string(v) ^ "\n")
                in
                  ListV []
                end
              | "!" => BoolV (not b)
              | _ => raise Impossible
          end
        | SeqV s =>
          let in
            case opr of "print" => 
                let 
                  val ignore = print(list2string(val2string, s) ^ "\n")
                in
                  ListV []
                end
              | "hd" => let in let in hd s end handle Empty => raise HDEmptySeq end
              | "tl" => let in let in SeqV (tl s) end handle Empty => raise TLEmptySeq end
              | "ise" =>
                let in
                  case s of
                      [] => BoolV true
                    | _ => BoolV false
                end
              | _ => raise Impossible
          end
        | ListV l =>
          let in
            case opr of "print" => 
                let 
                  val ignore = print(list2string(val2string, l) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | _ => raise Impossible
    end
  | eval (If (e1, e2, exp3)) (env:plcVal env) = 
    let in
      case eval e1 env of BoolV true => eval e2 env
        | BoolV false => eval exp3 env
        | _ => raise Impossible
    end
  | eval (Match (e1, matchList)) (env:plcVal env) = 
    let 
      val evalMatchVar = eval e1 env 
      (* Try matches will return the "cond -> expr" for which cond matches e1 *)
      fun tryMatches (matchVar, x::[]) env =
          let in
            case x of (SOME e2, exp3) => if matchVar = eval e2 env then exp3 else raise ValueNotFoundInMatch
              | (NONE, exp3) => exp3
          end
        | tryMatches (matchVar, x::xs) env =  let in
            case x of (SOME e2, exp3) => if matchVar = eval e2 env then exp3 else tryMatches (matchVar, xs) env
              | (NONE, exp3) => raise Impossible
          end
        | tryMatches (matchVar, _ ) env = raise Impossible
    in
      eval (tryMatches (evalMatchVar, matchList) env) env
    end
  | eval (Item (index, exp)) (env:plcVal env) =
    let
      fun getElementI (index, []) = raise Impossible
        | getElementI (index, (x::[])) = if index = 1 then x else raise Impossible
        | getElementI (index, (x::xs)) = if index = 1 then x else getElementI (index - 1, xs)
      val value = eval exp env
    in
      case value of ListV l => getElementI (index, l)
        | SeqV s => getElementI (index, s)
        | _ => raise Impossible
    end
  | eval (List []) (env:plcVal env) = ListV []
  | eval (List l) (env:plcVal env) = 
    let
      fun unroll (x::[]) = eval x env :: []
        | unroll (x::xs) = eval x env :: unroll xs
        | unroll _ = raise Impossible;
    in
      ListV (unroll l)
    end
  | eval (Let (var, e1, e2)) (env:plcVal env) =
    let
      val nEnv = (var, eval e1 env) :: env
    in
      eval e2 nEnv
    end
  | eval (Anon (typ, arg, exp)) (env:plcVal env) = Clos ("", arg, exp, env) (* We need to check if var can be found in the env of Anon *)
  | eval (Call (e1, e2)) (env:plcVal env) = 
    let
      fun get_arguments (List (x::[])) = [eval x env]
        | get_arguments (List (x::xs)) = [eval x env] @ get_arguments (List xs)
        | get_arguments (exp) = [eval exp env]
      val nEnv = [("$list", ListV (get_arguments e2))] @ env
      val f = eval e1 env
    in
      case f of Clos(name, var, exp, cEnv) =>
            let
              val ev = eval e2 nEnv
              val fEnv = (var, ev)::(name, f)::cEnv
            in
              eval exp fEnv
            end
        | _ => raise NotAFunc
    end
  | eval (Letrec (fName, argTyp, arg, funTyp, e1, e2)) (env:plcVal env) =
    let
      val nEnv = (fName, Clos(fName, arg, e1, env)) :: env
    in
      eval e2 nEnv
    end
  ;