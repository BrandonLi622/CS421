(* Brandon Li
   CPSC 421
   Assignment 5*)
  
signature SEMANT =
sig
  type ir_code
  val transprog : Absyn.exp -> {exp: ir_code, ty: Types.ty}
end

structure Semant : SEMANT = 
struct

  structure A = Absyn
  structure E = Env
  structure S = Symbol
  structure T = Types
  val error = ErrorMsg.error
  type ir_code = unit (* not used for the time being *)
  val loopDepth = ref 0;
  val temp = ref 0;

  (*** FILL IN DETAILS OF YOUR TYPE CHECKER PLEASE !!! ***)

  (*************************************************************************
   *                       UTILITY FUNCTIONS                               *
   *************************************************************************)

  
  (* Used for returns of g and h, gives you the type of the expression *)

  (* Debugging functions *)
  fun returnRecordFields((x,ty)::xs) = typeToString(ty) ^ returnRecordFields(xs)
    | returnRecordFields(nil) = ""

  and typeToString(T.NIL) = "NIL "
    | typeToString(T.INT) = "INT "
    | typeToString(T.STRING) = "STRING "
    | typeToString(T.UNIT) = "UNIT "
    | typeToString(T.NAME(sym,_)) = ("NAME(" ^ S.name(sym) ^ ")")
    | typeToString(T.ARRAY(ty,_)) = "ARRAY(" ^ typeToString(ty) ^ ")"
    | typeToString(T.RECORD(x,_)) = "RECORD(" ^ returnRecordFields(x) ^ ")"
  
  and printType(t) = print(typeToString(t) ^ "\n")

  fun findAndPrint(sym,env) = (case S.look(env,sym)
                                of NONE => (error 0 ("Cannot find " ^ S.name(sym)))
                                 | SOME(t) => printType(t))

  fun getType({exp, ty}) = ty

  (* Traces a name once to it's referred to type. Not intended to be used on other types *)
  fun traceNameOnce(T.NAME(_, ref(SOME(ty))), tenv, pos) = ty
    | traceNameOnce(T.NAME(_, ref(NONE)), _, _) = T.UNIT (* Just for testing equality anyways *)
    | traceNameOnce(ty,_,_) = ty;

  (* Takes the logical OR of a list of booleans *)
  fun orAny(b::bs) = b orelse orAny(bs)
    | orAny(nil) = false;
  
  (*  Determines if you have a loop through type declarations *)
  fun checkLoop(orig_type, tenv, pos) = 
        checkLoopHelper(orig_type, traceNameOnce(orig_type, tenv, pos), tenv, pos)
  
  and checkLoopHelper(T.NAME(sym1,x), T.NAME(sym2, ref(SOME(ty))), tenv, pos) = 
        if sym1 = sym2 
        then (error pos ("Cycle in definition of: " ^ S.name(sym1) ^ ". Type forced to INT"); true) 
        else checkLoopHelper(T.NAME(sym1,x), ty, tenv, pos)
    | checkLoopHelper(t1,t2,_,_) =  false

  (* Traces a type until it is no longer a T.NAME type. Note: Cannot handle loops *)
  fun actualTy(T.NAME(sym, ref(SOME(typ))), tenv) =  actualTy(typ, tenv)
    | actualTy(x, _) =  x
  
  (* Looks for symbol in tenv *)
  fun lookUpType((sym,pos), tenv) = (case S.look(tenv,sym)
                          	           of SOME(typ) => actualTy(typ, tenv)
                                        | NONE => (error pos ("Undefined type: " ^ S.name(sym)); 
                                                   T.INT));
  
  (* This will only be called on variables anyways *)
  (* Don't print any errors here because if you weren't able to find a variable that would have
     came up earlier in a different function *)
  fun isLoopVar(A.SimpleVar(sym,pos), env) = (case S.look(env, sym)
                                              of SOME(E.VARentry{access, ty, loopVar})  => loopVar
                                               | _ => false) (* Loop var cannot be a function *)
    | isLoopVar(_, _) = false;

  (*TODO: Apparently have to use the ! to check record and array equality*)
  fun sameType(T.RECORD(_, u1), T.RECORD(_, u2), _) = if u1 = u2 then true else false
    | sameType(T.ARRAY(_, u1), T.ARRAY(_, u2), _) = if u1 = u2 then true else false
    | sameType(T.NIL, T.RECORD(_,_), _) = true
    | sameType(T.RECORD(_,_), T.NIL, _) = true
    | sameType(T.STRING, T.STRING, _) = true
    | sameType(T.UNIT, T.UNIT, _) = true
    | sameType(T.NAME(a,u1), T.NAME(b,u2), tenv) = if u1 = u2 then true else false
    | sameType(t1, t2, _) = if t1 = t2 then true else false;

  (* Checks type equality by traceing names as well *)
  fun sameActualType(t1, t2, tenv) = sameType(actualTy(t1, tenv), actualTy(t2,tenv), tenv)
  
  (* Checks that the two things we are comparing are comparable using all operators except
     <> and = *)
  fun checkIneq({exp, ty = T.INT}, {exp=exp2, ty=T.INT}, pos) = {exp=(), ty=T.INT}
    | checkIneq({exp, ty = T.STRING}, {exp=exp2, ty=T.STRING}, pos) = {exp=(), ty=T.INT}
    | checkIneq({exp, ty}, {exp=exp2, ty=ty2}, pos) = 
        (error pos ("Should be both integer or both string type for this operator. " ^
                    "Left: " ^ typeToString(ty) ^ ", right: " ^ typeToString(ty2)); 
         {exp=(), ty=T.INT})
  

  (* Should all return T.INT *)
  (* Checks if we can use = or <> on the two types, which is possible for ints, strings, records
     and arrays *)
  fun checkCompEq ({exp=e1, ty=T.INT}, {exp=e2, ty=T.INT}, pos, _) = {exp=(), ty=T.INT}
    | checkCompEq ({exp=e1, ty=T.RECORD(r1,unique1)}, 
                   {exp=e2, ty=T.RECORD(r2,unique2)}, pos, tenv) =
        if unique1 = unique2
        then {exp=(), ty=T.INT}
        else (error pos ("Cannot compare these two record types"); {exp=(), ty=T.INT})
    | checkCompEq ({exp=e1, ty=T.ARRAY(a1,unique1)}, 
                   {exp=e2, ty=T.ARRAY(a2,unique2)}, pos, _) =
        if unique1 = unique2         
        then {exp=(), ty=T.INT}
        else (error pos ("Cannot compare these two array types"); {exp=(), ty=T.INT})
    | checkCompEq ({exp=e1, ty=T.NIL}, {exp=e2, ty=T.NIL}, pos, _) = 
        (error pos ("Cannot compare 2 NIL types"); {exp=(), ty=T.INT})
    | checkCompEq ({exp=e1, ty=T.STRING},{exp=e2, ty=T.STRING}, pos, _) = {exp=(), ty=T.INT}
    | checkCompEq ({exp=e1, ty=T.NIL}, {exp=e2, ty=T.RECORD(r,u)}, _, _) = {exp=(), ty=T.INT}
    | checkCompEq ({exp=e1, ty=T.RECORD(r,u)}, {exp=e2, ty=T.NIL}, _, _) = {exp=(), ty=T.INT}
    | checkCompEq ({exp=exp1, ty=T.NAME(symlist, u)}, {exp=exp2, ty=ty2}, pos, tenv) =
        checkCompEq({exp=exp1, ty=actualTy(T.NAME(symlist,u), tenv)}, 
                    {exp=exp2, ty=actualTy(ty2, tenv)}, pos, tenv)
    | checkCompEq ({exp=exp2, ty=ty2}, {exp=exp1, ty=T.NAME(symlist, u)}, pos, tenv)  =
        checkCompEq({exp=exp2, ty=actualTy(ty2, tenv)}, 
                    {exp=exp1, ty=actualTy(T.NAME(symlist,u), tenv)}, pos, tenv)   
    | checkCompEq ({exp=exp2, ty=ty1}, {exp=exp1, ty=ty2}, pos, tenv) = 
        (error pos ("Cannot use Eq or Neq on these two types. Left: " ^ typeToString(ty1) ^ 
                    ", right: " ^ typeToString(ty2));
         {exp=(), ty=T.INT}); 
                               
  (* TODO: Should I reduce all the way? *)
  fun paramToFormal(tenv, {var,typ,pos}) = 
    case S.look(tenv, typ)
      of SOME(ty) => ty
       | NONE => (error pos ("Undefined parameter type: " ^ S.name(typ));
                                                        T.INT)
  fun paramsToFormals(tenv, x::xs) = paramToFormal(tenv,x)::paramsToFormals(tenv,xs)
    | paramsToFormals(tenv, nil) = nil;

  (* Assume we're already inside the field of a record, try to fill in the missing NAME field *)
  fun secondPassRecord((name,T.NAME(sym, t))::xs, tenv, pos) = (t:= S.look(tenv,sym); 
                                                                secondPassRecord(xs,tenv,pos))
    | secondPassRecord((name,ty)::xs, tenv, pos) = secondPassRecord(xs,tenv,pos)
    | secondPassRecord(nil, tenv, pos) = tenv

  (* Looks for the name of a field inside of a record *)
  fun findID((sym,ty)::xs, id, pos) = if S.name(sym) = S.name(id) then ty else findID(xs, id, pos)
    | findID(nil,id, pos) = (error pos ("Could not find " ^ S.name(id) ^ " in record"); T.INT)

  (* Checks that names of function parameters are not duplicated *)
  fun checkDuplicateNames({name=n1, params=({var={name=name, escape=escape},typ=typ,pos=pos}::xs), 
                          result=r1, body=b1, pos=p1}, env, ls) = 
        ((if (List.exists (fn x => if x = name then true else false)  ls)
          then (error pos ("Duplicated field: " ^ S.name(n1)))
          else ()); checkDuplicateNames({name=n1,params=xs,result=r1,body=b1,pos=p1},env,n1::ls))
    | checkDuplicateNames({name=n3,params=nil,result,body,pos},env,ls) = ()

  (* Adds function parameters to an environment for evaluating a function body *)
  fun addParamsToEnv({name=n1, 
                     params=({var={name=name, escape=escape},typ=typ,pos=pos}::xs),  
                     result=r1, body=b1, pos=pos1_3}, env, tenv) = 
        let val newType = lookUpType((typ,pos),tenv)
        in addParamsToEnv({name=n1,params=xs,result=r1,body=b1,pos=pos1_3}, 
        (S.enter(env, name, E.VARentry({access=(), ty=newType, loopVar=false}))), tenv)
        end
    | addParamsToEnv({name=n3,params=nil,result=r3,body=b3,pos=p3},env, tenv) = env

  (* Mainly for if the two types are record types, here we are assuming that the
     two types are already the same. Also is useful for finding type of an if-then-else *)
  fun selectType(T.UNIT,ty2) = ty2
    | selectType(T.NIL, ty2) = ty2
    | selectType(ty1,T.UNIT) = ty1
    | selectType(ty1,T.NIL) = ty1
    | selectType(ty1,ty2) = ty2


 (**************************************************************************
  *                   TRANSLATING TYPE EXPRESSIONS                         *
  *                                                                        *
  *              transty : (E.tenv * A.ty) -> (T.ty * A.pos)               *
  *************************************************************************)
  
  (* If we're here we're assuming that the type is not in the table *)
  fun transty (tenv, A.ArrayTy(id, pos)) = 
        (case S.look(tenv, id)
            of SOME(t) => (T.ARRAY(t, ref ()), pos)
             | NONE => (error pos ("Undefined type: " ^ S.name(id)); 
                        (T.ARRAY(T.INT, ref ()), pos))) 
    | transty (tenv, A.NameTy(sym,pos)) =
        (case S.look(tenv,sym)
            of SOME(t) =>  (T.NAME(sym, ref(SOME(t))), pos)
             | NONE => (error pos ("Undefined type: " ^ S.name(sym)); (T.UNIT, pos))) 
    | transty (tenv, A.RecordTy(nil)) = (T.RECORD(nil, ref ()), 0) (* Hmm, position *)
    | transty (tenv, A.RecordTy({name,typ,pos}::xs)) =
        (checkDuplicateFields({name=name,typ=typ,pos=pos}::xs, nil);
         (T.RECORD(makeRecPairs(tenv, {name=name,typ=typ,pos=pos}::xs), ref ()), pos))
             
  (* This function is only used in transty, converts the abstract syntax tfield to
     pairs usable for T.RECORD *)
  and makeRecPairs (tenv, {name,typ,pos}::xs) =
        (case S.look(tenv,typ)
            of SOME(t) => (name, t)::makeRecPairs(tenv, xs)
             | NONE => (error pos ("Undefined type: " ^ S.name(typ)); 
                        (name, T.UNIT)::makeRecPairs(tenv, xs)))
    | makeRecPairs (tenv, nil) = nil

  (* Records cannot have duplicate fields, so this functino checks that *)
  and checkDuplicateFields({name,typ,pos}::xs, seenNames) = 
        (if (List.exists (fn x => if x = name then true else false) seenNames)
         then (error pos ("Duplicate field: " ^ S.name(name)))
         else ();
         checkDuplicateFields(xs, name::seenNames))
    | checkDuplicateFields(nil, seenNames) = ()


 (**************************************************************************
  *                   TRANSLATING EXPRESSIONS                              *
  *                                                                        *
  *  transexp : (E.env * E.tenv) -> (A.exp -> {exp : ir_code, ty : T.ty})  *
  **************************************************************************)
  fun transexp (env, tenv) expr =
    let fun g (A.OpExp {left,oper=A.EqOp,right,pos}) = checkCompEq(g left,g right,pos, tenv)    
          | g (A.OpExp {left,oper=A.NeqOp,right,pos}) = checkCompEq(g left, g right, pos, tenv)
          | g (A.OpExp {left,oper,right,pos}) = checkIneq (g(left), g(right), pos)
          | g (A.VarExp(x)) = h(x) 
          | g (A.NilExp) = {exp=(), ty=T.NIL} 
          | g (A.RecordExp {typ,fields,pos}) =
                (case actualTy(getOpt(S.look(tenv, typ), T.NIL), tenv)
                    of T.RECORD(sym_list, u) =>
                            checkRecType(sym_list,fields,T.RECORD(sym_list,u),pos)
                     | _ => (error pos ("Not defined as a record type"); {exp=(), ty=T.INT})) 
          | g (A.ArrayExp {typ,size,init,pos}) = 
                ((case actualTy(getOpt(S.look(tenv, typ), T.NIL), tenv)
                  of T.ARRAY(arrtype, u) =>
                    let 
                      val {exp,ty} = g(init)
                      val x = ref false
                    in
                      (if sameActualType(arrtype,ty,tenv)
                      then ()
                      else (x:=true; error pos ("Array does not match type definition. Returning "
                                                ^ "INT"));

                      if sameActualType(getType(g(size)), T.INT, tenv)
                      then ()
                      else (x:=true; error pos ("Exp inside brackets should return an int. " ^
                                                 "Returning INT"));
                      if !x
                      then {exp=(), ty=T.INT}
                      else {exp=(), ty=T.ARRAY(arrtype,u)})
                    end
                  | T.NAME(a,b)  => ({exp=(), ty=actualTy(T.NAME(a,b), tenv)})
                  | _    => (error pos ("Incorrect ArrayExp, not a correct array type. Returning " ^
                                         "INT");
                             {exp=(), ty=T.INT})))
          | g (A.IntExp(_)) = {exp=(), ty=T.INT}
          | g (A.StringExp(_,_)) = {exp=(), ty=T.STRING}   
          | g (A.SeqExp(nil)) = {exp=(), ty=T.UNIT}            
          | g (A.SeqExp((exp,pos)::nil)) = g exp
          | g (A.SeqExp((exp,pos)::xs)) = (g exp; g(A.SeqExp(xs)))
          | g (A.AssignExp({var,exp,pos})) =
                let
                    val {exp=exp1, ty=ty1} = h(var);
                    val {exp=exp2, ty=ty2} = g(exp)
                in
                    (if (not(sameActualType(ty1,ty2,tenv))) 
                     then error pos ("Bad assignment, different types given. " ^
                                     "Left: " ^ typeToString(ty1) ^ ", " ^
                                     "Right: " ^ typeToString(ty2))  
                     else (); 
                     (if isLoopVar(var,env)
                      then error pos ("Cannot assign to loop variable")
                      else ());
                     {exp=(), ty=T.UNIT})
                end
 
          | g (A.IfExp({test, then', else', pos})) = 
               if sameActualType(getType(g(test)), T.INT, tenv)
               then
                    case else'
                        of NONE      => let
                                          val ty1 = T.UNIT
                                          val ty2 = getType(g(then'))
                                        in
                                          if sameActualType(ty1,ty2,tenv)
                                          then {exp=(), ty=ty1}
                                          else (error pos ("In if-then, then must return nothing. "
                                                           ^ "Got: " ^ typeToString(ty2));
                                                {exp=(), ty=T.UNIT})
                                        end
                         | SOME(exp) => let
                                          val ty1 = getType(g(then'))
                                          val ty2 = getType(g(exp))
                                          val return = selectType(ty1,ty2)
                                        in
                                          if sameActualType(ty1,ty2,tenv)
                                          then {exp=(), ty=selectType(ty1,ty2)}
                                          else (error pos ("Then, else must return same type. " ^
                                                           "Then: " ^ typeToString(ty1) ^
                                                           ", Else: " ^ typeToString(ty2) ^
                                                           ". Returning " ^ typeToString(return));
                                                {exp=(), ty=return})
                                        end
               else (error pos ("If condition must result in an integer"); {exp=(), ty=T.UNIT})

          | g (A.WhileExp({test, body, pos})) =
               (if sameActualType(getType(g(test)), T.INT,tenv)
                then () 
                else (error pos ("Loop condition must result in an integer"));
                loopDepth := !loopDepth + 1;
                (let
                  val typ = getType(g(body))
                 in
                  if sameActualType(typ, T.UNIT, tenv)
                  then ()
                  else (error pos ("Expression inside loop body must return unit"))
                 end);

                loopDepth := !loopDepth - 1;      
                {exp=(), ty=T.UNIT})

          | g (A.ForExp({var={name,escape}, lo, hi, body, pos})) = 
               (if sameActualType(getType(g(lo)), T.INT,tenv) andalso 
                   sameActualType(getType(g(hi)), T.INT,tenv)
                then ()
                else (error pos ("Loop variable bounds must evaluate to integers"));
                loopDepth := !loopDepth + 1;
                (let
                  val typ = getType(transexp (S.enter(env, name, E.VARentry{access=(), 
                                                                            ty = getType(g(lo)), 
                                                                            loopVar = true}),
                                              tenv) body)
                in
                  if sameActualType(typ, T.UNIT,tenv)
                  then ()
                  else (error pos ("Loop body must return unit, returns " ^ typeToString(typ)))
                end);


                loopDepth := !loopDepth - 1; 
                {exp=(), ty=T.UNIT})

          | g (A.BreakExp(pos)) = (
                (if !loopDepth > 0 
                 then () 
                 else (error pos ("Can only break inside loop"))); 
                 {exp=(), ty=T.UNIT})
          
          | g (A.AppExp({func,args,pos})) =
                (case S.look(env,func)
                    of SOME(E.FUNentry{level, label, formals, result}) =>
                        (funParamsCorrect(args,formals,pos); {exp=(), ty=result} )  
                     | SOME(E.VARentry{access,ty,loopVar}) => 
                        (error pos ((S.name func) ^" is a variable, not a function. Returning INT");
                         {exp=(), ty=T.INT})
                     | NONE => (error pos ("Undefined function: " ^ S.name(func) ^ ". Returning " ^
                                           "INT"); 
                                {exp=(), ty=T.INT}))
          
          (* Returns whatever the body returns. It expects a sequence, which is checked in the
             syntactic checker *)
          | g (A.LetExp({decs,body,pos})) =
                let
                    val (new_env, new_tenv) = transdecs(env,tenv,decs)
                in
                    (transexp (new_env, new_tenv) body) 
                end
          

        (* function dealing with "var", may be mutually recursive with g *)
        and h (A.SimpleVar (id,pos)) = 
                (case S.look(env, id)
                     of SOME(E.VARentry({access, ty, loopVar})) => {exp=(), ty=actualTy(ty, tenv)}
                     | NONE => (error pos ("Undefined variable: " ^ S.name(id)); {exp=(), ty=T.INT})
                     | _ => (error pos (S.name(id) ^ " is a function, not a var"); 
                             {exp=(), ty=T.INT}))
          | h (A.SubscriptVar (v,exp,pos)) = 
                (case h(v)
                    of {exp=e1, ty=T.ARRAY(ty,_)} => 
                          (if sameActualType(T.INT, getType(g(exp)), tenv)
                           then {exp=(), ty=actualTy(ty, tenv)}
                           else (error pos ("Array suscript should be an INT, is: " ^
                                            typeToString(getType(g(exp))) ^ ". Returning INT"); 
                                {exp=(), ty=T.INT}))
                     | {exp=e1, ty} => (error pos ("Type is "^typeToString(ty) ^", not an array." ^
                                                   " Returning INT");  
                                     {exp=(), ty=T.INT}))
	        | h (A.FieldVar (v,id,pos)) = 
	            (case h(v)
	                of {exp, ty=T.RECORD(x::xs,_)} => 
                        {exp=(), ty=actualTy(findID(x::xs, id, pos), tenv)} 
	                 | {exp, ty} => (error pos ("Type is " ^ typeToString(ty) ^ ", not a record. " ^
                                              "Returning INT");  
                                   {exp=(), ty=T.INT}))

        (* Checks if the fields are correct, same return style as g() and h(), if possible *)
        (* First argument is the table stuff, second argument is the actual *)
        and checkRecType(nil,nil,target_typ, _) = {exp=(), ty=target_typ}
          | checkRecType((sym1,ty1)::xs, (sym2,init,pos)::ys, target_typ, overall_pos) =
                let
                    val {exp=_,ty=ty2} = g(init)
                in
                    (if sameActualType(ty1, ty2, tenv) 
                     then ()
                     else (error pos ("Record field '" ^ S.name(sym1) ^ "' does not match type. " ^
                                      "Expected: "^typeToString(ty1)^", got: "^typeToString(ty2)));

                     if (S.name sym1) = (S.name sym2)
                     then ()
                     else (error pos ("Record field '" ^ (S.name sym2) ^ "' expected, '" ^ 
                                     (S.name sym1) ^ "' given"));
                     checkRecType(xs,ys,target_typ, overall_pos)) (* Just to minimize errors *)
                end
          | checkRecType(_,_,target_typ, overall_pos) = 
              (error overall_pos ("Mismatched number of fields. Returning INT"); 
               {exp=(), ty=T.INT})
        
        
        (* Checks if the function paramaters are correct and gives error if not. No return value *)
        and funParamsCorrect(nil,nil,_) = ()
          | funParamsCorrect(exp::xs,ty::ys,overall_pos) =
                (if sameActualType(getType(g(exp)), ty,tenv)
                 then ()
                 else error overall_pos ("Incorrect type of function argument. " ^
                                         "Expected: " ^ typeToString(ty) ^ "got: " ^
                                          typeToString(getType(g(exp))));
                funParamsCorrect(xs,ys,overall_pos))
          | funParamsCorrect(_,_,overall_pos) = 
              error overall_pos ("Wrong number of arguments for this function"); 
          
     in g expr
    end

 (**************************************************************************
  *                   TRANSLATING DECLARATIONS                             *
  *                                                                        *
  *  transdec : (E.env * E.tenv * A.dec) -> (E.env * E.tenv)               *
  **************************************************************************)
  and transdec (env, tenv, A.VarDec({var={name,escape},typ,init,pos})) = 
    (case typ
      of SOME((symbol,pos2)) =>
      (case S.look(tenv, symbol)
        of SOME(ty) => if sameActualType(getType(transexp (env,tenv) init), ty, tenv) 
                       then (S.enter(env, name, E.VARentry({access=(),ty=ty,loopVar=false})), tenv)
                       else (error pos ("Type doesn't match initial value for: " ^ S.name(name)); 
                             (S.enter(env, name, E.VARentry({access=(),ty=ty,loopVar=false})),tenv))
        | NONE => (error pos ("Undefined type: " ^ S.name(symbol)); 
                   (S.enter(env, name, E.VARentry({access=(), ty=T.INT, loopVar=false})), tenv))) 
      | NONE => 
         (case getType(transexp (env,tenv) init)
            of T.NIL => (error pos ("Must specify record type for: " ^ S.name(name)); 
                         (S.enter(env, name, E.VARentry({access=(),ty=T.NIL,loopVar=false})), tenv))
             | t => (S.enter(env, name, E.VARentry({access=(),ty=t, loopVar=false})), tenv)))
    

    | transdec (env, tenv, A.FunctionDec({name,params,result,body,pos}::xs)) =
       (temp := !loopDepth;
        loopDepth := 0;
        let
            val formals = paramsToFormals(tenv,params)
            val test = checkDuplicateNames({name=name, params=params, result=result, body=body, 
                                            pos=pos}, env, nil)
            val body_env = addParamsToEnv({name=name, params=params, result=result, body=body, 
                                           pos=pos}, env, tenv)
            val {exp,ty} = (transexp (body_env,tenv) body)
        in
            (loopDepth := !temp;
             (case result
                of SOME(symbol, pos2) => 
                  (let
                    val ty2 = lookUpType((symbol,pos2), tenv);
                   in
                    if sameActualType(ty,ty2, tenv)
                    then (transdec(S.enter(env, name, E.FUNentry({level=(),label=(),formals=formals,
                                                                  result=actualTy(ty2,tenv)})), 
                          tenv, A.FunctionDec(xs)))
                    else (error pos ("Function body type does not match return type. " ^
                                   "Expected: " ^ typeToString(ty2) ^ ", got: " ^ typeToString(ty));
                    (transdec(S.enter(env, name, 
                    E.FUNentry({level=(),label=(),
                      formals=formals, 
                      result=ty2})),
                    tenv, A.FunctionDec(xs))))   
                   end)

              (* If type is not specified, it should return UNIT *)
              | NONE => 
                (if sameActualType(ty,T.UNIT,tenv)
                 then ()
                 else (error pos ("Function body does not match return type. " ^
                                  "Expected: "^typeToString(T.UNIT)^", got: "^typeToString(ty)));
                 (transdec(S.enter(env, name, E.FUNentry({level=(),label=(), formals=formals, 
                                                          result=T.UNIT})),  
                           tenv, A.FunctionDec(xs))))))            
        end)
    | transdec (env, tenv, A.FunctionDec(nil)) = (env, tenv) (* Never will get here on first call *)
    | transdec (env, tenv, A.TypeDec({name,ty,pos=pos2}::xs)) =
        let
            val (typ, pos3) = transty(tenv, ty)
        in
            (case S.look(tenv, name)
                of SOME(T.NAME(s,t)) =>  (t:= SOME(typ); (* Put it in table first *)
                                         if checkLoop(T.NAME(s,t), tenv, pos2)
                                         (* Then remove it if it causes a loop *)
                                         then t:= SOME(T.INT)  (* Just a default value *)
                                         else ();
                                         transdec(env,tenv, A.TypeDec(xs)))
                | _ => (env, tenv))
        end
    | transdec (env, tenv, A.TypeDec(nil)) = (env, tenv)


  (* Fills in placeholders and also checks for duplicates *)
  and decFirstPass(_, _, env,tenv,A.VarDec({var,typ,init,pos})) = (env,tenv)
    | decFirstPass(_, _, env,tenv,A.FunctionDec(nil)) = (env,tenv)
    | decFirstPass(dupenv,duptenv,env,tenv,
                   A.FunctionDec({name,params,result=NONE,body,pos}::fundecs)) = 
       (case S.look(dupenv,name)
           of SOME(x) => error pos ("Redefinition of function: " ^ (S.name(name)))
            | NONE => ();
        let val funEntry = E.FUNentry{level=(), label=(), formals=paramsToFormals(tenv,params), 
                                      result= T.UNIT}
        in decFirstPass(S.enter(dupenv,name,funEntry), duptenv, S.enter(env, name, funEntry), tenv, 
                        A.FunctionDec(fundecs)) 
        end)
    | decFirstPass(dupenv, duptenv, env,tenv,
                   A.FunctionDec({name,params,result=SOME((symbol, p)),body,pos}::fundecs)) =              
         (case S.look(dupenv,name)
           of SOME(x) => error pos ("Redefinition of function: " ^ (S.name(name)))
            | NONE => ();   
         let val funEntry = E.FUNentry{level=(), label=(), formals=paramsToFormals(tenv,params), 
                                       result=lookUpType((symbol,p), tenv)}
         in decFirstPass(S.enter(dupenv,name,funEntry),duptenv, S.enter(env, name, funEntry), tenv, 
                         A.FunctionDec(fundecs))
         end)
    | decFirstPass(_, _, env,tenv,A.TypeDec(nil)) = (env,tenv)
    | decFirstPass(dupenv, duptenv, env,tenv,A.TypeDec({name, ty, pos}::typdecs)) = 
         (case S.look(duptenv, name)
            of SOME(x) => error pos ("Redefinition of type: " ^ (S.name(name)))
             | NONE => ();
         let val typeEntry = T.NAME(name, ref(NONE))
         in decFirstPass(dupenv,S.enter(duptenv,name,typeEntry),env, S.enter(tenv, name, typeEntry), 
                         A.TypeDec(typdecs))
         end)


  (*** transdecs : (E.env * E.tenv * A.dec list) -> (E.env * E.tenv) ***)
  and transdecs (env,tenv,nil) = (env, tenv)
    | transdecs (env,tenv,dec::decs) =
        let 
          val (env1, tenv1) = decFirstPass(S.empty, S.empty,env,tenv,dec); 
          val (env2,tenv2) = transdec (env1,tenv1,dec)
        in transdecs (env2,tenv2,decs)
        end

  (*** transprog : A.exp -> {exp : ir_code, ty : T.ty} ***)
  fun transprog prog = transexp (E.base_env, E.base_tenv) prog

end  (* structure Semant *)