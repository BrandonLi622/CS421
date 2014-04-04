signature ENV =
sig
  type access
  type level
  type label
  type ty

  datatype enventry 
    = VARentry of {access: access, ty: ty}
    | FUNentry of {level: level, label: label, formals: ty list, result: ty}

  type tenv = ty Symbol.table
  type env = enventry Symbol.table

  val base_tenv : tenv
  val base_env : env
end

functor EnvGen(Translate: TRANSLATE) : ENV =
struct

  structure S = Symbol
  structure T = Types
  structure Tr = Translate
  type access = Tr.access
  type level = Tr.level
  type label = Temp.label
  type ty = T.ty

  datatype enventry 
    = VARentry of {access: access,
                   ty: ty}
    | FUNentry of {level: level,
                   label: label, (* the label of the function *)
                   formals: ty list,
                   result: ty}

  type tenv = ty Symbol.table

  type env = enventry Symbol.table

  fun enter((symbol, entry),env) = Symbol.enter(env, symbol, entry)
  
  (* here you need to add all primtive types into the base_tenv *)
  val base_tenv = foldr enter Symbol.empty [
    (Symbol.symbol("int"), Types.INT),
    (Symbol.symbol("string"),Types.STRING)
  ] 
  (* here you need to add all primitive library functions into the base_env *)
  (* all primitive library functions are added to the base level with their
  * names as the label *)
  val outermost = #1(Tr.newLevel({parent=Tr.outermost, formals=[]:bool list}))
  val base_env = foldr enter S.empty [
    (Symbol.symbol("print"), FUNentry{level=outermost, label=Temp.namedlabel("print"), formals=[Types.STRING],
    result=Types.UNIT}),
    (Symbol.symbol("flush"), FUNentry{level=outermost, label=Temp.namedlabel("flush"), formals=[],
    result=Types.UNIT}),
    (Symbol.symbol("getchar"), FUNentry{level=outermost, label=Temp.namedlabel("getchar"), formals=[],
    result=Types.STRING}),
    (Symbol.symbol("ord"), FUNentry{level=outermost, label=Temp.namedlabel("ord"), formals=[Types.STRING],
    result=Types.INT}),
    (Symbol.symbol("chr"), FUNentry{level=outermost, label=Temp.namedlabel("chr"), formals=[Types.INT],
    result=Types.STRING}),
    (Symbol.symbol("size"), FUNentry{level=outermost, label=Temp.namedlabel("size"), formals=[Types.STRING],
    result=Types.INT}),
    (Symbol.symbol("substring"), FUNentry{level=outermost,
    label=Temp.namedlabel("substring"), formals=[Types.STRING,
    Types.INT, Types.INT], result=Types.STRING}),
    (Symbol.symbol("concat"), FUNentry{level=outermost, label=Temp.namedlabel("concat"), formals=[Types.STRING,
    Types.STRING], result=Types.STRING}),
    (Symbol.symbol("not"), FUNentry{level=outermost, label=Temp.namedlabel("not"), formals=[Types.INT],
    result=Types.INT}),
    (Symbol.symbol("exit"), FUNentry{level=outermost, label=Temp.namedlabel("exit"), formals=[Types.INT],
    result=Types.UNIT})
  ]

end  (* structure Env *)
  
