signature SEMANT =
sig
  type ir_code
  val transprog : Absyn.exp -> Frame.frag list
end

functor SemantGen (Register: REGISTER_STD) : SEMANT = 
struct

  structure A = Absyn
  structure Tr = TranslateGen(Register)
  structure E = EnvGen(Tr) 
  structure S = Symbol
  structure T = Types
  val error = ErrorMsg.error
  type ir_code = Tr.gexp (* not used for the time being *)

  (* nestlevel used to keep track of control-loop nesting
  *  for BREAK statements *)
  val nestlevel = ref 0
  val printdepth = 8
  (* FATAL is used if there was a compiler error, not if the
  *  input program had an error *)
  exception FATAL 
  
  val isSubExp = ref false
  (*************************************************************************
   *                       UTILITY FUNCTIONS                               *
   *************************************************************************)
  (*
  *                         ERROR HANDLING
  *                                                                        *)
  (* typeOption is a debugging function that turns an Option Type into a 
  * printable string *)
  fun typeOption(ty,i) =
    if i < printdepth then
      (case !ty of
            NONE    => "NONE"
          | SOME(t) => (typeString(t,i+1)))
      else
        "..."
  (* typeString: turns a type into a printable string *)
  and typeString(ty,i) =
    if i < printdepth then
    (case ty of
       T.RECORD(styl,_) => ("RECORD of {" ^
        concat(map (fn x => x ^ ", ") (map (fn x => typeString(x,i+1)) (map #2 styl)))^"}")
      |T.NIL            =>  "NIL"
      |T.INT            =>  "INT"
      |T.STRING         =>  "STRING"
      |T.ARRAY(t,_)     =>  ("ARRAY of (" ^ typeString(t,i+1) ^ ")")
      |T.NAME(sy,t)     =>  (S.name sy ^ " of (" ^ typeOption(t,i+1) ^")")
      |T.UNIT           =>  " T.UNIT"
      |T.READONLY(t)    => "READONLY of " ^ typeString(t, i+1))
    else
      "..."
  (* typeerror: gives an error message when when two types do not agree *)
  fun typeerror(l, r, pos, s) =
    error pos (s ^ "\n\t" ^ typeString(l,0) ^ "\n\t" ^
        typeString(r,0) ^ "\n")
  (* nameType: makes sure it is a name, then returns the type 
  * associated with it and returns T.UNIT on failure *)
  fun nameType(T.NAME(s, t)) =
    (case !t of
          SOME(ty) => ty
        | _ => T.UNIT)
   | nameType(_) = T.UNIT
  (* lookup a type in an envronment *)
  fun tlookup(tenv, n, pos) =
    (case S.look (tenv, n) of
        SOME ty2 => ty2
      | NONE => (error pos ("type is undefined: " ^ S.name n); T.UNIT))
  fun funlookup(env, n) =
    (case S.look (env, n) of
        SOME v =>
          (case v of
              E.FUNentry(v) => v
            | _ => (error ~1 ("function is not funentry " ^ S.name n); raise FATAL))
      | NONE => (error ~1 ("function not found: " ^ S.name n); raise FATAL))
      (* return UNIT type on error *)
  (* areRecDups: Checks if there are duplicate keys in a record with the same
  * name *)
  fun areRecDups([] :A.tfield list) = false
  |  areRecDups({name, typ, pos}::r) = 
    (* check if each key appears later in the record *)
    if (List.exists(fn ({name=n,typ=t,pos=p}) => name = n) r) then
      (error pos ("Record Declaration: Multiple Keys with the Same Name " 
        ^ (S.name name)); true)
    else areRecDups(r)
  (* areTydups checks for duplicate type names
  *  in mutually recursive type declarations *)
  fun areTydups([] :A.symbol list, [] :A.pos list) = false
    | areTydups(s::rs, p::rp) =
    if (List.exists(fn sym => sym = s) rs) then
      (error p ("Type Declarations: Multiple Types with the Same Name: "
        ^ (S.name s)); true)
    else areTydups(rs, rp)
    | areTydups(_,_) = raise FATAL
  (* areFunDups checks for duplicate function names
  *  in mutually recursive function declarations *)
  fun areFunDups([] :A.symbol list, [] :A.pos list) = false
    | areFunDups(s::rs, p::rp) =
    if (List.exists(fn sym => sym = s) rs) then
    (error p ("Function Declarations: Multiple Functions with the Same Name: "
        ^ (S.name s)); true)
    else areFunDups(rs, rp)
    | areFunDups(_,_) = (error ~1 "Function Declarations: Compiler Error";
      raise FATAL)
  (* Take tfields in types and transform them into (name, type) pairs *)
  (* Used in RecordExp *)
  fun recordTypes(fields, myenv) =
    map (fn({name=name,typ=typ,pos=pos})=>
      (let 
        val typ = tlookup(myenv, typ, pos)
       in 
          (name, typ) 
       end)) fields
  (* TYPE ANALYSIS *)
  (* actual_ty gets rid of all T.NAMES *) 
  fun actual_ty (T.NAME (s,ty), pos) =
    (case !ty of
        SOME t => (actual_ty (t, pos))
      | NONE => (error pos ("Parsing Type Information: Compiler Error: "
          ^ "T.NAME should never have NONE in actual_ty"); raise FATAL))
    | actual_ty(T.READONLY(ty), pos) = actual_ty(ty, pos)
    | actual_ty(t,pos) = t (* DESTROY THE NAMES *)

  (* shallowcompare: just checks for the same type name *)
  fun shallowcompare(l, r, p) =
    case (l, r) of
        (T.RECORD(_,_), T.RECORD(_,_)) => (l = r) 
      | (T.RECORD(_,_), T.NIL) =>  (true)
      | (T.NIL, T.RECORD(_,_)) => (true)
      | (T.NAME(_,_), T.NAME(_,_)) =>
          shallowcompare(actual_ty(l,p),actual_ty(r,p),p)
          (*ignore NAME's when comparing*)
      | (_,T.NAME(_,_)) => shallowcompare(l, actual_ty(r,p),p)
      | (T.NAME(_,_), _) => shallowcompare(actual_ty(l,p), r,p)
      | (T.READONLY(a), _) => shallowcompare(a, r, p)
      | (_, T.READONLY(b)) => shallowcompare(l, b, p)
      | (T.NIL, T.NIL) => false
      | (_, _) => (l = r)

  (* comparelist: compares two lists of types for equivalence *)
  fun comparelist(h1::r1, h2::r2, pos, n :int) =
    (if not (shallowcompare(h1,h2,pos)) then 
      (typeerror(h1,h2,pos,
        "Function Call: Argument types do not agree: Parameter " ^ str(chr(n)));
        comparelist(r1, r2, pos, n+1)) else comparelist(r1,r2,pos, n+1))
  | comparelist([], [], pos, n) = ()
  | comparelist(_,_, pos, n) =
    (error pos 
      ("Function Call: Unequal Number of Function Arguments: At Parameter"
        ^ str(chr(n))); ())
  
  (* ARITHMETIC OPERATOR GROUPINGS *)
  fun isArithOp(oper) =
    oper = A.PlusOp orelse oper = A.MinusOp orelse
    oper = A.TimesOp orelse oper = A.DivideOp

  fun isCompOp(oper) =
    oper = A.LtOp orelse oper = A.LeOp orelse
    oper = A.GtOp orelse oper = A.GeOp

  fun isEqOp(oper) =
    oper = A.EqOp orelse oper = A.NeqOp

  (* Fast functions for checking types *)
  fun checkInt(ty,pos) =
    (if ty=T.INT then true else false)

  fun checkString(ty, pos) =
    (if ty=T.STRING then true else false)

  (* shallowcheckRecord: checks two records to see if they are type equivalent *)
  fun shallowcheckRecord(l, r, pos) = 
    (case r of
        T.NIL => true
      | _ => (if shallowcompare(l, r, pos) then true else 
          (typeerror(l,r,pos,"Record: Type Incongruity:"); false)))

  (* checkArray: checks to see if two arrays are typically equivalent *)
  fun checkArray(T.ARRAY(ty1, uniq1), T.ARRAY(ty2, uniq2), pos) =
    ((if shallowcompare(ty1,ty2,pos) then () else
        typeerror(ty1,ty2,pos,"Arrays must be the same type"));
     true)
    | checkArray(T.ARRAY(ty1, uniq1), T.NIL, pos) = true
    | checkArray(T.NIL, T.ARRAY(ty2, uniq2), pos) = true
    | checkArray(t1,t2, pos) = (
      typeerror(t1,t2,pos,"Array Expression: Must be Arrays:"); false)

 (**************************************************************************
  *                   TRANSLATING TYPE EXPRESSIONS                         *
  *                                                                        *
  *              transty : (E.tenv * A.ty) -> (T.ty * A.pos)               *
  *************************************************************************)
  (* transty takes an environment and a type and it returns the internal
  *  tiger representation of that type *)
  fun transty (tenv :E.tenv, A.ArrayTy(id, pos)) =
      T.ARRAY(tlookup(tenv, id, pos), ref())
    | transty (tenv, A.RecordTy tfields) =
      (areRecDups tfields;
        T.RECORD(recordTypes(tfields,tenv), ref()))
    | transty (tenv, A.NameTy (n, pos)) = 
        tlookup(tenv,n,pos)

 (**************************************************************************
  *                   TRANSLATING EXPRESSIONS                              *
  *                                                                        *
  *  transexp : (E.env * E.tenv * Tr.level) -> (A.exp -> {exp : ir_code, ty : T.ty})  *
  **************************************************************************)
  fun transexp (env :E.env, tenv :E.tenv, level :Tr.level) expr =
    let fun g (A.NilExp)  = {exp=Tr.nilExp(), ty=T.NIL}
      | g (A.VarExp var) = h var
      | g (A.IntExp i) = {exp=Tr.intExp(i), ty=T.INT}
      | g (A.StringExp (str,pos)) = {exp=Tr.stringExp(str), ty=T.STRING}
      | g (A.OpExp {left=l,oper=oper,right=r,pos=pos}) =
      let
        val l' = g l
        val al = actual_ty(#ty l',pos)
        val r' = g r
        val ar = actual_ty(#ty r',pos)
      in
        if isArithOp(oper) then
          (if checkInt(al, pos) andalso checkInt(ar, pos) then
            {exp=Tr.intOpExp(oper, #exp l', #exp r'), ty=T.INT} else
              (typeerror(#ty l', #ty r', pos, "Arithmetic: Integers Required:");
                {exp=Tr.intExp(1), ty=T.INT}))
        else if isCompOp(oper) then
          (case al of
              T.INT =>
                (if checkInt(ar, pos) then {exp=Tr.intOpExp(oper, #exp l', #exp r'), ty=T.INT} else
                  (typeerror (#ty l', #ty r', pos, "Comparison: Integer Required:");
                {exp=Tr.intExp(1), ty=T.INT}))
            | T.STRING =>
                (if checkString(ar, pos) then {exp=Tr.stringOpExp(oper, #exp l', #exp r'), ty=T.INT} else
                  (typeerror (#ty l', #ty r', pos, "Comparison: String Required:");
                {exp=Tr.intExp(1), ty=T.INT}))
            | _ =>
                (typeerror (#ty l', #ty r', pos,
                  "Comparison is not supported for this type");
                {exp=Tr.intExp(1), ty=T.INT}))
        else if isEqOp(oper) then (** TODO: Reference comparison Translate **)
          (case al of
              T.INT =>
                (if checkInt(ar, pos) then {exp=Tr.intOpExp(oper, #exp l', #exp r'), ty=T.INT} else
                  (typeerror (#ty l', #ty r', pos, "Equality: Integer Required:");
                {exp=Tr.intExp(1), ty=T.INT}))
            | T.STRING =>
                (if checkString(ar, pos) then {exp=Tr.stringOpExp(oper, #exp l', #exp r'), ty=T.INT} else
                  (typeerror (#ty l', #ty r', pos, "Equality: String Required:");
                {exp=Tr.intExp(1), ty=T.INT}))
            | T.RECORD(symtys, uniq) =>
                (if shallowcompare(#ty l', #ty r', pos) then () else
                  (typeerror (#ty l', #ty r', pos,
                    "Equality: Record Types Do Not Match"));
                {exp=Tr.pOpExp(oper, #exp l', #exp r'), ty=T.INT})
            | T.ARRAY(ty, uniq) =>
                (if shallowcompare(#ty l', #ty r', pos) then () else
                  (typeerror(#ty l', #ty r', pos,
                    "Equality: Array Types Do Not Match"));
                    {exp=Tr.pOpExp(oper, #exp l', #exp r'), ty=T.INT})
            | T.NIL =>
                (if shallowcompare(T.NIL, #ty r', pos) then () else
                  (typeerror (#ty l', #ty r', pos,
                    "Equality: Nil Cannot be compared with this type"));
                {exp=Tr.pOpExp(oper, #exp l', #exp r'), ty=T.INT})
            | _ =>
                  (typeerror (#ty l',#ty r',pos,
                    "Equality is not supported for these types:");
                {exp=Tr.intExp(1), ty=T.INT}))
        else
          (typeerror(#ty l', #ty r', pos,"Operator Unknown:");
            {exp=Tr.intExp(1), ty=T.INT})
      end
      | g (A.ArrayExp{typ, size, init, pos}) =
          let
            val mytype = tlookup(tenv, typ, pos) (* lookup the type *)
            val t      = nameType(mytype) (* what is the type after the names *)
            val s = g size (* figure out the size *)
            val i = g init (* figure out the initial *)
            val exp = Tr.arrayVar((#exp s),(#exp i))
          in
            (checkInt(#ty s, pos); 
              (case actual_ty(t,pos) of
                    T.ARRAY(ty,_) => (if shallowcompare(ty, #ty i, pos) then () 
                      else (typeerror (ty,#ty i,pos,"Invalid Array Expression:")))
                  | _ => (typeerror (t,#ty i,pos,"Invalid Array Expression:")));
             {exp=exp, ty=mytype}) (* TODO: how should I handle this *)
          end
      | g (A.RecordExp {typ,fields,pos}) =
        let
          val typ' = tlookup(tenv, typ, pos)
          val t = nameType(typ')
        in
          (case t of
              T.RECORD(sytys, uniq) =>
                let
                  val varkeys = map #1 fields
                  val varposes = map #3 fields
                  val rectypes = map #2 sytys
                  val varexpres = map (fn x => g (#2 x)) fields
                  val vartypes = map #ty varexpres
                  val reckeys = map #1 sytys 
                in
                  (* check for equality in types *)
                  (map 
                    (fn ((x,y),p) => 
                      if shallowcompare(x, y, p) then ()
                        else (typeerror(x,y,p,"Record Type Error:")))
                  (ListPair.zip(ListPair.zip(rectypes, vartypes), varposes)));
                  (* check for equality between field names *)
                  (map
                    (fn ((x,y),p) =>
                      if x = y then () else
                        (error p
                        ("Record Fields (" ^ S.name y ^ ") should be (" ^ S.name
                        x ^ ")")))
                  (ListPair.zip(ListPair.zip(reckeys,varkeys), varposes)));
                  {exp=Tr.recordExp(map #exp varexpres), ty=typ'}
                end
              | _ =>
                  (error pos "Record Expression: Not a Record";
                  {exp=Tr.nilExp(), ty=T.UNIT}))
        end
      | g (A.AppExp{func, args, pos}) =
        let
          val args = (map g args)
          val actualtys = (map #ty args)
          val params = (map #exp args)
        in
          (case S.look(env,func) of
                SOME(E.FUNentry{level, label, formals, result}) =>
                  let
                    val _ = (comparelist(actualtys, formals, pos, 0))
                    val call = Tr.callExp(level, label, params)
                    val _ = print ("Func: " ^ Symbol.name label ^ ": Found\n")
                    (*val _ = Tr.printtree(call)*)
                  in
                    {exp=call, ty=result}
                  end
              | _ =>
                  (error pos ("Function Call: Function does not exist: " ^
                  S.name func);
                  {exp=Tr.nilExp(), ty=T.UNIT}))
        end
      | g (A.SeqExp(seqexp)) = 
        if seqexp = nil then {exp=Tr.nilExp(), ty=T.UNIT} else 
          (let
            val seq = map (fn x => g (#1 x)) seqexp
            val exps = map #exp seq
          in
            {exp=Tr.seqExp(exps), ty=(#ty (List.last(seq)))}
          end)
      | g (A.AssignExp{var, exp, pos}) =
        let
          val {exp=left, ty=tyleft} = h var
          val {exp=right, ty=tyright} = g exp
          val _ = 
            (case tyleft of
              T.READONLY(l) => 
              (error pos "Assigment: Cannot Assign to a Loop Variable")
                |  _ => ())
        in 
          if shallowcompare(tyleft,tyright,pos) then
            {exp=Tr.assignExp(left, right), ty=T.UNIT} (* used to be tyleft *)
          else
            (typeerror (tyleft, tyright, pos,
              "Types in ASSIGNMENT expressions must agree");
            {exp=Tr.nilExp(), ty=T.UNIT})
        end
      | g (A.IfExp{test=test,then'=thn,else'=els, pos=pos}) =
        (let
          val tst = g test
          val thn = g thn
        in
          (* make sure that the test is an integer *)
          (checkInt(#ty tst,pos);
           (case els of
              SOME(e) =>
                let 
                  val el = g(e)
                in
                  (* make sure then and else clause agree in type *)
                  if shallowcompare(#ty el, #ty thn, pos) then
                    ({exp=Tr.ifExp(#exp tst, #exp thn, #exp el), ty=(#ty thn)})
                  else
                    (error pos "If-Then-Else Expressions must agree in Types";
                    {exp=Tr.nilExp(), ty=T.UNIT})
                end
            | _ => (
              if shallowcompare(#ty thn, T.UNIT, pos) then
                {exp=Tr.ifThenExp(#exp tst, #exp thn), ty=(#ty thn)}
              else
                (error pos 
                  "If-Then expression with no ELSE must return UNIT type";
                 {exp=Tr.nilExp(), ty=T.UNIT}))))
        end)
      | g (A.WhileExp{test, body,pos}) =
        let
          val test' = g test
          val old_break = !Tr.breakLabel
          val _ = Tr.breakLabel := Tr.getBreak()
          val _ = (nestlevel := !nestlevel+1)
          val body' = g body
          val _ = Tr.breakLabel := old_break 
          val _ = (nestlevel := !nestlevel-1)
        in
          checkInt(#ty test', pos);
          if (shallowcompare (#ty body', T.UNIT, pos)) then
            {exp=Tr.whileExp(#exp test', #exp body'), ty=T.UNIT}  
          else
            (error pos "BODY in WHILE loop must return UNIT type.";
            {exp=Tr.nilExp(), ty=T.UNIT})
        end
      | g (A.ForExp{var, lo, hi, body, pos}) =
        let
          val access = Tr.allocInFrame (level, false)
          val old_break = !Tr.breakLabel
          val _ = Tr.breakLabel := Tr.getBreak()
          val l = 
            (let val a = g lo 
              in 
                if shallowcompare(#ty a, T.INT,pos) then 
                  a 
                else
                  (error pos "LOW value in FOR loop must be INT";
                  {exp=Tr.intExp(0), ty=T.INT})
              end)
          val env' =
            S.enter(env, #name var, E.VARentry{access=access, ty=T.READONLY(T.INT)})
          val h =
            (let val a = g hi
              in 
                if shallowcompare(#ty a,T.INT,pos) then 
                  a 
                else
                  (error pos "HI value in FOR loop must be INT";
                  {exp=Tr.intExp(0), ty=T.INT})
              end)
          val _ = (nestlevel := !nestlevel+1)
          val b = transexp(env', tenv, level) body
          val _ = (nestlevel := !nestlevel-1)
          val _ = Tr.breakLabel := old_break
        in
          if (shallowcompare (#ty b, T.UNIT, pos)) then
            {exp=Tr.forExp(Tr.simpleVar(access, level), #exp l, #exp h, #exp b), ty=T.UNIT}
          else
            (error pos "Body in FOR loop must return UNIT type";
            {exp=Tr.nilExp(), ty=T.UNIT})
        end
      | g (A.BreakExp(pos)) =
        if !nestlevel <> 0 then
          {exp=Tr.breakExp(), ty=T.UNIT}
        else
          (error pos "Break Statement only permitted in FOR or WHILE loop";
           {exp=Tr.nilExp(), ty=T.UNIT})
      | g (A.LetExp{decs,body,pos}) =
        let 
          val explist = []
          val (env', tenv', explist')=transdecs(env, tenv, decs,level, explist)
          (*val _ = print "Let Statement\n"
          val _ = map (fn x => Tr.printtree(x)) explist'*)
          val {exp, ty}=transexp(env', tenv', level) body
          (*val _ = Tr.printtree(exp)*)
          val l = Tr.letExp(explist', exp)
          (*val _ = print "Let Statement: Tr.letExp\n"
          val _ = Tr.printtree(l)*)
        in
          {exp=l, ty=ty} 
        end

        (* function dealing with "var", may be mutually recursive with g *)
    and h (A.SimpleVar (id, pos)) =
        (case S.look(env, id) of
              SOME(E.VARentry{access, ty}) =>
                {exp=Tr.simpleVar(access, level), ty=ty}
              | _ => 
                  (error pos 
                    ("Undefined Simple Variable: " ^ S.name id);
                {exp=Tr.nilExp(), ty=T.INT})) (* default to int *)
	  | h (A.FieldVar (v,id,pos)) =
        let
          val {exp, ty} = h v
        in
        (* Add way of checking record consistancy *)
        (case actual_ty(ty, pos) of
            T.RECORD(stys, uniq) =>
              (let
                fun contains((s, t)::r, id, pos, offset) =
                    if s = id then
                      (* look up the type if it is in the list *)
                      (t, offset)
                    else contains(r, id, pos, offset+1)
                  | contains(_, id, pos, offset) =
                    (error pos ("Field Variable: key is not a valid entry: " 
                      ^ S.name id); (T.UNIT, 0))
                val (t, offset) = contains(stys, id, pos, 0)
               in
                {exp=Tr.fieldVar(exp, offset), ty=t}
               end)
            | t => (error pos 
              ("Field variable must be within RECORD: " ^ S.name id ^ 
               " but is witin " ^ typeString(t,0));
              {exp=Tr.nilExp(), ty=T.UNIT}))
        end
	  | h (A.SubscriptVar (var,exp,pos)) =
        let
          val var' = h var
          val v = #exp(var')
          val nty = #ty(var')
          val ty  = nameType(nty)
          val exp'=g exp
          val e = #exp(exp')
          val _ = isSubExp := true 
        in
          (checkInt(#ty exp', pos);
          (case ty  of
                T.ARRAY(typ,_)=>{exp=Tr.subVar(v, e), ty=typ}
              | _ =>
                (error pos ("Subscript Expression: Variable must be Array"); 
                {exp=Tr.nilExp(), ty=T.UNIT})))
        end

     in g expr
    end

 (**************************************************************************
  *                   TRANSLATING DECLARATIONS                             *
  *                                                                        *
  *  transdec : (E.env * E.tenv * A.dec * Tr.exp list) -> (E.env * E.tenv)               *
  **************************************************************************)
  (* favor the left hand side of arguments *)
  (* explist: on page 167: must be returned to keep track of assignment expressions *)
  and transdec (env :E.enventry Symbol.table,
                tenv, A.VarDec({var, typ, init, pos}), level, explist :Tr.gexp list) =
    let
        (* ty is the type of the right hand side of the assginement *)
        val {exp, ty} = transexp(env, tenv, level) init
        (* create access on this frame level *)
        val access = Tr.allocInFrame(level, false)
        val assign_exp = Tr.assignExp(Tr.simpleVar(access, level), exp)
        val explist' = explist@[assign_exp]
        (*val _ = print "Printing explist\n"
        val _ = Tr.printtree(assign_exp)*)
    in
    (case typ of
        (* Check to make sure that the type exists *)
        SOME(s, pos') =>
          (case S.look (tenv, s) of
              NONE => (error pos ("Undefined Type: " ^ S.name s);
                (S.enter(env, (#name var), E.VARentry{access=access, ty=T.UNIT}), tenv, explist))
            | SOME ty' => (
              (* Make sure both sides of the equal signs agree in type *)
              if (shallowcompare(ty, ty', pos)) andalso ty' <> T.NIL then
                (S.enter(env, (#name var), E.VARentry{access=access, ty=ty'}), tenv,
                  explist')
              else 
                (typeerror 
                  (ty,ty',pos, "Variable declaration types disagree");
              (S.enter(env, (#name var), E.VARentry{access=access, ty=ty'}), tenv, explist))))
      
      | NONE => (if ty <> T.NIL then
          (S.enter(env, (#name var), E.VARentry{access=access, ty=ty}), tenv, explist')
                  else
                    (* make sure type is not underdefined *)
                    (error pos
                      "No Type Specified: Untyped Variable cannot be NIL";
                    (S.enter(env, (#name var), E.VARentry{access=access,ty=T.UNIT}),
                    tenv, explist))))
      (* return the new environment with this variable in it *)
    end
    | transdec (env, tenv, A.FunctionDec(declist), level, explist) = 
      let
        (* store the previous nesting level *)
        (* BREAK statements are not valid in the first level of functions *)
        val oldnestlevel :int = !nestlevel
        val _ = nestlevel := 0
        val fundefs :Tr.gexp list ref = ref []
        fun checkNameDups(name::names : A.symbol list, pos::poses) =
          if (List.exists (fn x => name = x) names) then
            (error pos "Invalid Duplicate Name in Function Declarations";
              checkNameDups(names, poses))
          else
            checkNameDups(names, poses)
          | checkNameDups([], []) = ()
          | checkNameDups(_,_) = ErrorMsg.impossible "check name dups: names and positions do not match up"

        (* check for duplicate names between the functions*)
        val names = map #name declist
        val poses = map #pos declist
        val _ = checkNameDups(names, poses)
        (* keep track of function levels *)
        val funlevels : Tr.level list ref = ref []
        fun makeFunction(fundec: A.fundec, env) =
          let
            (* get the result of the function
                type is implicity checked *)
            val result_type =
              (case (#result fundec) of
                NONE => T.UNIT
              | SOME(ty, pos) => tlookup(tenv, ty, pos))

            (* params need to remember name and type *)
            val params' =
              (map
                (fn({var as {name, escape}, typ, pos}) =>
                  {name=name, ty=tlookup(tenv, typ, pos)})
              (#params fundec))

            (* check for duplicate names within this function declaration *)
            val poses = (#pos fundec)::(map #pos (#params fundec))
            val names = (#name fundec)::(map #name (map #var (#params fundec)))

            val _ = checkNameDups(names, poses)

            (* add in parameter checking here, types and names *)

            (* pull out the escapes *)
            val formals = 
              (map (fn({var as {name, escape}, typ, pos})=>
                (!escape))
              (#params fundec))

            (* create a new label for this function *)
            val label = Temp.newlabel()

            (* create a new level for this function *)
            val new_level = 
              Tr.newLevel{parent=level, formals=formals}

            (* update the global level tracker *)
            val _ = funlevels := (!funlevels)@[#1 new_level]
            (*val _ = print ("Func: " ^ Symbol.name label ^ " created\n")*)
          in
            Symbol.enter(
              env,
              (#name fundec),
              E.FUNentry{level=(#1 new_level),
                         label=label,
                         formals=(map #ty params'),
                         result=result_type}
              )
          end
        (* create the new function environments for each of them *)
        val funenvs = (foldl makeFunction env declist)

        fun addparam ({var as {name, escape}, typ, pos}, funenv) =
          let
            val access = Tr.allocInFrame(hd(!funlevels), true)
            val ty = tlookup(tenv, typ, pos)
          in
            S.enter(funenv, name, E.VARentry{access=access, ty=ty})
          end

        (* evaluate the interior of the functions in order *)
        fun evalbody(fundec : A.fundec) =
          let
            val func = funlookup(funenvs, #name fundec)
            val label = #label(func)
            (*val _ = print ("Evaluating function Body\n")
            val _ = print (Symbol.name label ^ "\n") *)
            (* make my new environment *)
            val myenv = foldl addparam funenvs (#params fundec)
            (* evaluate the function body *)
            val my_level = hd(!funlevels)
            val {exp, ty} = transexp(myenv, tenv, my_level) (#body fundec)
            (* Update the funlevels list to show that I have used one *)
            val _ = (funlevels := tl(!funlevels))
            (* labeling now handled by procEntryExit *)
            (*val body = Tr.funLabel(label, exp)*)
            (*val _ = Tr.printtree(body)*)
            (* enter and exit based on this body *)
            val _ = Tr.procEntryExit({level=my_level, body=exp, label=label})
          in
            fundefs := (!fundefs)@[exp]
          end

        val checkbodies = (map evalbody declist)
        val fundefs' = !fundefs
        val explist' = (fundefs')@explist
      in
        (* add the prototype function entries to the table *)
        nestlevel := oldnestlevel;
        (funenvs, tenv, explist)
      end
    | transdec (env, tenv, A.TypeDec tydecs, level, explist) =
    let
      (* parse out names poses and types *)
      val names = map #name tydecs
      val poses = map #pos tydecs
      val types = map #ty tydecs
      val _ = areTydups(names, poses)
      (* function for adding type prototypes to the table *)
      fun addProto(name, myenv) =
        (S.enter(myenv, name, T.NAME(name,ref (S.look(tenv, name)))))
      (* Add type prototypes to the table *)
      val tenv' = foldl addProto tenv names
      val tigtys = map (fn t => transty (tenv', t)) types
      fun update((name, tigty),pos) =
        let
          val looked = S.look(tenv', name)
          val (SOME(T.NAME(_,t))) = S.look(tenv',name)
        in
          t := SOME(tigty)
        end
      val _ = app update (ListPair.zip (ListPair.zip(names, tigtys), poses))
      fun advance(t) =
        (case t of
            SOME(T.NAME(s, ty)) =>
              (case !ty of
                SOME(typ) => !ty
              | _ => NONE)
          |  _ => NONE)
      fun reccheck(T.NAME(s1,t1), T.NAME(s2,t2),pos) =
        let
          val slow = advance(!t1)
          val fast = advance(advance(!t2))
        in
          (case (slow,fast) of
                (SOME(a), SOME(b)) => 
                  (if a = b then 
                    false
                  else
                    (reccheck(a, b, pos)))
              | (_,_) => true)
        end
        | reccheck(_,_,pos) = true
      fun recchecker(SOME(t1), SOME(t2), pos) =
          if reccheck(t1, t2, pos) then () else  (typeerror(t1,t2,pos,"Mutually Recursive Types"))
        | recchecker(_,_,_) = ()
        val _ = map (fn x => recchecker(SOME(x), advance(SOME(x)), hd poses)) tigtys
    in
      (env, tenv', explist)
    end

  (*** transdecs : (E.env * E.tenv * A.dec list) -> (E.env * E.tenv) ***)
  and transdecs (env,tenv,nil,level, explist) = (env, tenv, explist)
    | transdecs (env,tenv,dec::decs,level, explist) =
	let
      val (env',tenv', explist') = transdec (env,tenv,dec,level, explist)
 	in 
      transdecs(env',tenv',decs,level, explist')
	end

  (*** transprog : A.exp -> {exp : ir_code, ty : T.ty} ***)
  fun transprog prog =
    let
      val base_level = Tr.newLevel{parent=Tr.outermost, formals=[]}
      val {exp, ty} = transexp (E.base_env, E.base_tenv, #1 base_level) prog
      (*val _ = print "transprog\n"
      val _ = Tr.printtree(exp)*)
      (* exp is the internals of the function: or in this case main body *)
      (* body is the expression returned with error handling *)
      val body = if !isSubExp then Tr.mainErrors(exp) else exp
      val _ = Tr.procEntryExit({level=(#1 base_level), body=body, label=Tr.mainLabel})
    in
      (* Tr.unNx *)
      Tr.getResult()
    end
end  (* structure Semant *)
  

