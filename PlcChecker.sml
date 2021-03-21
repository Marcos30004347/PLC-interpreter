(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun teval (e) (env:plcType env) = 
  case e of 
  (ConI _) => IntT
  | (ConB _) => BoolT
  | (ESeq s) => let in
      case s of
          SeqT t => SeqT t
        | _ => raise EmptySeq
    end
  | (Var v) => let in lookup env v handle UnkownSymbol => raise UnkownSymbol end
  | (List l) =>
    let
      fun evalAll (h::[]) = (teval h env)::[]
        | evalAll (h::t) = (teval h env)::evalAll t
        | evalAll _ = []
      val lst = evalAll l
    in
      ListT lst
    end
  | (Item (index, e)) =>
    let
      fun get (i, []) = raise ListOutOfRange
        | get (i, (x::[])) = if i = 1 then x else raise ListOutOfRange
        | get (i, (x::xs)) = if i = 1 then x else get (i - 1, xs)
      val vType = teval e env
    in
      case vType of
          ListT l => get(index, l)
        | _ => raise OpNonList
    end
  | (Prim1(oper, e)) =>
    let
      val e_type = teval e env
    in
      case oper of
          "!" => if e_type = BoolT then BoolT else raise UnknownType
        | "-" => if e_type = IntT then IntT else raise UnknownType
        | "hd" => let in
            case e_type of
                SeqT t => t
              | _ => raise UnknownType
          end
        | "tl" => let in
            case e_type of
                SeqT t => SeqT t
              | _ => raise UnknownType
          end
        | "ise" => let in
            case e_type of
                SeqT t => BoolT
              | _ => raise UnknownType
          end
        | "print" => ListT []
        | _ => raise UnknownType
    end
  | (Prim2(oper, e1, e2)) =>
    let
      val e1_type = teval e1 env
      val e2_type = teval e2 env
    in
      case oper of
          "&&" => if e1_type = BoolT andalso e2_type = BoolT then BoolT else raise UnknownType
        | "::" => let in
            case (e1_type, e2_type) of
                (IntT, ListT []) => SeqT IntT
              | (IntT, SeqT t2) => if t2 = IntT then SeqT t2 else raise NotEqTypes
              | (BoolT, ListT []) => SeqT BoolT
              | (BoolT, SeqT t2) => if t2 = BoolT then SeqT t2 else raise NotEqTypes
              | (ListT t, ListT []) => SeqT (ListT t)
              | (ListT t, SeqT t2) => if t2 = ListT t then SeqT t2 else raise NotEqTypes
              | _ => raise UnknownType
          end
        | "+" => if e1_type = IntT andalso e2_type = IntT then IntT else raise UnknownType
        | "-" => if e1_type = IntT andalso e2_type = IntT then IntT else raise UnknownType
        | "*" => if e1_type = IntT andalso e2_type = IntT then IntT else raise UnknownType
        | "/" => if e1_type = IntT andalso e2_type = IntT then IntT else raise UnknownType
        | "<" => if e1_type = IntT andalso e2_type = IntT then BoolT else raise UnknownType
        | "<=" => if e1_type = IntT andalso e2_type = IntT then BoolT else raise UnknownType
        | "=" => if e1_type = e2_type andalso (e1_type = IntT orelse e1_type = BoolT) then BoolT else raise NotEqTypes
        | "!=" => if e1_type = e2_type andalso (e1_type = IntT orelse e1_type = BoolT) then BoolT else raise NotEqTypes
        | ";" => e2_type
        | _ => raise UnknownType
    end
  | (If(e1, e2, e3)) =>
    let
      val e1_type = teval e1 env
      val e2_type = teval e2 env
      val e3_type = teval e3 env
    in
      case e1_type of
          BoolT => if e2_type = e3_type then e2_type else raise DiffBrTypes
        | _ => raise IfCondNotBool
    end
  | (Let(var, e1, e2)) =>
    let
      val e1_type = teval e1 env
      val tmp_env = (var, e1_type) :: env
    in
      teval e2 tmp_env
    end
  | (Anon(t, arg, e)) => 
    let
      val tmp_env = (arg, t) :: env
      val e_type = teval e tmp_env
    in
      FunT (t, e_type)
    end
  | (Letrec(fun_name, arg_type, arg, funTyp, e1, e2)) =>
    let
      val rec_env = (fun_name, FunT (arg_type, funTyp))
      val arg_env = (arg, arg_type)
      val e1_type = teval e1 (rec_env :: arg_env :: env)
      val e2_type = teval e2 (rec_env :: env)
    in
      if e1_type = funTyp then e2_type else raise WrongRetType
    end
  | (Call(e2, e1)) =>
    let
      val e1_type = teval e1 env
      val e2_type = teval e2 env
    in
      case e2_type of
          FunT (arg_typee, resultType) => 
            if e1_type = arg_typee then resultType else raise CallTypeMisM
        | _ => raise NotFunc
    end
  | (Match(e1, l)) =>
    let
      val cond = teval e1 env
      val res = (#2 (hd l))
      val res_type = teval res env
      fun match (Match(e1, l)) (env:plcType env) =
          let in
            case l of
                x::[] => let in
                    case x of
                        (SOME e2, e3) => 
                          if (teval e3 env) = res_type then
                            if cond = (teval e2 env) then 
                              teval e3 env 
                            else raise MatchCondTypesDiff
                          else raise MatchResTypeDiff
                      | (NONE, e3) => if (teval e3 env) = res_type then res_type else raise MatchResTypeDiff
                  end
              | x::xs => let in
                    case x of
                        (SOME e2, e3) => 
                          if (teval e3 env) = res_type then
                            if cond = (teval e2 env) then
                              match (Match(e1, xs)) env 
                            else raise MatchCondTypesDiff
                          else raise MatchResTypeDiff
                      | _ => raise UnknownType
                  end
              | _ => raise NoMatchResults
          end
        | match _ _ = raise UnknownType
    in
      match (Match(e1, l)) env
    end