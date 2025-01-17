(*Environ*)

exception UnkownSymbol

type 'a env = (string * 'a) list

fun lookup [] id = raise UnkownSymbol
  | lookup ((k:string, v)::t) id = if k = id then v else lookup t id;
