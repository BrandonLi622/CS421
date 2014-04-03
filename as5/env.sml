(* Brandon Li
   CPSC 421
   Assignment 5*)

signature ENV =
sig
  type access
  type level
  type label
  type ty

  datatype enventry 
    = VARentry of {access: access, ty: ty, loopVar : bool}
    | FUNentry of {level: level, label: label, formals: ty list, result: ty}

  type tenv = ty Symbol.table
  type env = enventry Symbol.table

  val base_tenv : tenv
  val base_env : env
end

structure Env : ENV =
struct

  structure S = Symbol
  structure T = Types

  type access = unit   (* not used for the time being *) (* What does this mean? *)
  type level = unit    (* not used for the time being *)
  type label = unit    (* not used for the time being *)
  type ty = T.ty

  datatype enventry 
    = VARentry of {access: access, ty: ty, loopVar : bool}
    | FUNentry of {level: level, label: label, formals: ty list, result: ty}

  type tenv = ty Symbol.table

  type env = enventry Symbol.table

  (* here you need to add all primitive types into the base_tenv *)
  val base_tenv = let
                    val blank_env = S.empty;
                    val new_env = S.enter(blank_env, S.symbol("int"), T.INT);
                    val new_env = S.enter(new_env, S.symbol("string"), T.STRING);
                    
                    
                    (*For Testing!!! *)
                    (*val new_env = S.enter(new_env, S.symbol("rec"), T.RECORD([(S.symbol "a", T.STRING), (S.symbol "b", T.INT)], ref ()))
                    val new_env = S.enter(new_env, S.symbol("arr"), T.ARRAY(T.INT, ref ()))*)
                  in
                    new_env
                  end;
 
  (* here you need to add all primitive library functions into the base_env *)
  val base_env = let
                    val blank_env = S.empty;
                    val new_env = S.enter(blank_env, S.symbol("print"), 
                                          FUNentry({level = (), label = (), formals = T.STRING::nil, 
                                                    result = T.UNIT}));
                    val new_env = S.enter(new_env, S.symbol("flush"), 
                                          FUNentry({level = (), label = (), formals = nil,
                                                    result = T.UNIT}));
                    val new_env = S.enter(new_env, S.symbol("getchar"), 
                                          FUNentry({level = (), label = (), formals = nil,
                                                    result = T.STRING}));
                    val new_env = S.enter(new_env, S.symbol("ord"), 
                                          FUNentry({level = (), label = (), formals = T.STRING::nil,
                                                    result = T.INT}));
                    val new_env = S.enter(new_env, S.symbol("chr"), 
                                          FUNentry({level = (), label = (), formals = T.INT::nil,
                                                    result = T.STRING}));
                    val new_env = S.enter(new_env, S.symbol("size"), 
                                          FUNentry({level = (), label = (), formals = T.STRING::nil,
                                                    result = T.INT}));    
                    val new_env = S.enter(new_env, S.symbol("substring"), 
                                          FUNentry({level = (), label = (), 
                                                    formals = T.STRING::T.INT::T.INT::nil,
                                                    result = T.STRING}));
                    val new_env = S.enter(new_env, S.symbol("concat"), 
                                          FUNentry({level = (), label = (), 
                                                    formals = T.STRING::T.STRING::nil,
                                                    result = T.STRING}));
                    val new_env = S.enter(new_env, S.symbol("not"), 
                                          FUNentry({level = (), label = (), formals = T.INT::nil,
                                                    result = T.INT}));
                    val new_env = S.enter(new_env, S.symbol("exit"), 
                                          FUNentry({level = (), label = (), formals = T.INT::nil,
                                                    result = T.UNIT}));
                    
                    
                    (* For Testing! *)
                    (*
                    val new_env = S.enter(new_env, S.symbol("a"),
                                          VARentry({access=(),ty=T.ARRAY(T.ARRAY(T.ARRAY(T.ARRAY(T.INT, ref ()), ref ()), ref ()), ref ())}));
                    val new_env = S.enter(new_env, S.symbol("b"),
                                          VARentry({access=(),ty=T.RECORD((S.symbol("c"), T.INT)::nil, ref ())}))
                                          
                    val new_env = S.enter(new_env, S.symbol("c"),
                                          VARentry({access=(),ty=T.INT}))*)
                    
                 in
                    new_env
                 end;

end  (* structure Env *)
  
