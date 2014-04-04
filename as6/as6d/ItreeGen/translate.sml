(* translate.sml *)

signature TRANSLATE = 
sig 
  type level
  type access
  type frag
  val mainLabel : Temp.label
  val breakLabel : Temp.label ref
  val getBreak : unit -> Temp.label
  val outermost : level
  val newLevel : {parent: level, formals: 'a list} -> 
                                     (level * (('a * access) list))
  val allocInFrame : level * bool -> access
   (*
   * val allocInRegister : unit -> access
   *)
  val getResult : unit -> frag list

  type gexp
  val printtree : gexp -> unit
  val printTreeStm : Tree.stm -> unit
  val nilExp : unit -> gexp
  val intExp : int -> gexp
  val stringExp : string -> gexp
  val simpleVar : (access * level) -> gexp 
  val intOpExp : (Absyn.oper * gexp * gexp) -> gexp
  val stringOpExp : (Absyn.oper * gexp * gexp) -> gexp
  val pOpExp : (Absyn.oper * gexp * gexp) -> gexp
  val callExp : (level * Temp.label * gexp list) -> gexp
  val seqExp : gexp list -> gexp
  val assignExp : (gexp * gexp) -> gexp
  val ifThenExp : (gexp * gexp) -> gexp
  val ifExp : (gexp * gexp * gexp) -> gexp
  val recordExp : gexp list -> gexp
  (* Why can't I put Tree.label here *)
  val whileExp : (gexp * gexp) -> gexp 
  val forExp : (gexp * gexp * gexp * gexp) -> gexp
  val breakExp : unit -> gexp
  val letExp : (gexp list * gexp) -> gexp
  val procEntryExit : {level : level, body : gexp, label : Temp.label} -> unit
  val arrayVar : (gexp * gexp) -> gexp
  val fieldVar : (gexp * int) -> gexp 
  val subVar : gexp * gexp -> gexp
  val funLabel : Temp.label * gexp -> gexp
  val mainErrors : gexp -> gexp
end (* signature TRANSLATE *)

functor TranslateGen(Register : REGISTER_STD) : TRANSLATE = 
struct
  structure A = Absyn
  structure F  = Frame
  structure Tr = Tree
  structure T  = Temp
  structure Er = ErrorMsg
  val mainLabel = T.namedlabel("tigermain")
  val breakLabel = ref (T.newlabel())
  fun getBreak() = T.newlabel()
  val indexOutOfBounds = T.newlabel()
  (* TODO: What is sl_offset:
      UPDATE 3/26/12: Professor Zhao says that it might be used by Registers
      frame: the current stack frame
      sl_offset: the offset within this stack frame
      parent: the level of the parent stack frame (initially should be top)  *)
  datatype level = LEVEL of {frame : F.frame,              
                             sl_offset : int,
                             parent : level} * unit ref
                 | TOP
  type access = level * int  (* where to access this variriable level and offset? *)
  (* might need to be changed later: Not the same as F.access *)
  type frag = F.frag
  (* frags: the list of itree fragments that we will return *)
  val frags = ref([] : frag list)
  (* getResult returns the fragments generated after main *)
  fun getResult() = !frags
  (* newLevel takes in a parent level and a list of formals.
        It returns a new frame level.
        BUG: sl_offset is currently arbitrary
        BUG: newFrame may not be properly implemented *)
  fun newLevel({parent=l, formals=f}) =
    let
      val frame_offset = F.newFrame(List.length(f))
      val frame  = #1(frame_offset)
      val offset = #2(frame_offset)
      (* Generate the return list of ('a * access) *)
      val retlist = ListPair.zip(f, List.tabulate(List.length(f), fn x => (l, 0)))
    in
      if null offset then
        (LEVEL({frame=frame, sl_offset=0,parent=l}, ref ()), retlist) else
    (LEVEL({frame=frame, sl_offset=hd(offset), parent=l}, ref ()), retlist)
    end
  (* outermost is the outermost level: the child of TOP *)
  val outermost = #1 (newLevel({parent=TOP : level, formals=[] : bool list}))
  (* leveleq: takes a two levels and returns whether the levels are equal
      the levels are compared by their unique reference which
      acts as a unique id for that level *)
  fun leveleq (LEVEL(_,uid1), LEVEL(_, uid2)) = uid1=uid2
    | leveleq (_, _) = false
  (* allocInFrame: Attempts to allocate in a frame 
      BUG: Currently access is being determined randomly, perhaps allocate in frame Frame.allocInFrame *)
  fun allocInFrame(TOP, _) = (Er.impossible("cannot allocate local variable at TOP"); (TOP, 0))
    | allocInFrame(l as LEVEL({frame=frame, sl_offset=offset, parent=parent}, r), isParam) =
      (l, F.allocInFrame(frame, isParam))
  (* allocInRegister: Attempts to allocate space in a register 
      BUG: Have no clue what is happening here *)
  fun allocInRegister() = (TOP, 0)
  datatype gexp = Ex of Tr.exp
                | Nx of Tr.stm
                | Cx of T.label * T.label -> Tr.stm
  (* UTILITY FUNCTIONS: For converting among Ex, Nx and Cx expressions *)
  fun seq [] = Er.impossible("Sequence cannot be empty")
    | seq [a] = a
    | seq(a::r) = Tr.SEQ(a, seq r)
  (* unEx: converts gexp into a Tree.exp *)
  fun unEx(Ex e) = e
    | unEx(Nx s) = Tr.ESEQ(s, Tr.CONST 0)
    | unEx(Cx genstm) =
      let val r = T.newtemp()
          val t = T.newlabel() and f = T.newlabel()
      in Tr.ESEQ(seq[Tr.MOVE(Tr.TEMP r, Tr.CONST 1),
                       genstm(t,f),
                       Tr.LABEL f,
                       Tr.MOVE(Tr.TEMP r, Tr.CONST 0),
                       Tr.LABEL t],
                    Tr.TEMP r)
      end
  (* unNx: converts gexp into a Tr.stm *)
  fun unNx(Ex e) = Tr.EXP(e)
    | unNx(Nx s) = s
    | unNx(Cx genstm) =
      let
        val t = T.newlabel()
        val f = t
      in
        (* Takes two labels and turns into a statement *)
        Tr.SEQ(genstm(t,f), Tr.LABEL t)
      end
  (* unCx: converts gexp into conditional *)
  fun unCx(Ex(Tr.CONST 0)) =
      (* Jump takes an expression and a list of labels *)
      (fn (t, f) => Tr.JUMP(Tr.NAME f, [f]))
    | unCx(Ex(Tr.CONST _)) =
      (fn (t,f) => Tr.JUMP(Tr.NAME t, [t]))
    | unCx(Ex e) =
      (fn(t,f) => Tr.CJUMP(Tr.TEST(Tr.NE, e, Tr.CONST 0), t, f))
    | unCx(Nx _) =
      Er.impossible("Cannot unCx an nx")
    | unCx(Cx genstm) = genstm
  fun printtree(tree) = (print "\nPrinting Tree:\n"; Printtree.printtree(TextIO.stdOut, unNx tree))
  fun printTreeStm(tree) = (print "\nPrinting Tree: \n"; Printtree.printtree(TextIO.stdOut, tree))
  (* Function for assigning a left variable to a right variable 
      improves reading in more complex code, this is different
      than assignExp which returns code for an assignment expression
      in an itree *)
  fun assign(l, r) = Tr.MOVE(l, r)
  (** CONSTANTS SECTION **)
  (* intExp: takes an integer and returns an integer constant
      (which is an expression), for the itree *)
  fun intExp (i) = Ex (Tr.CONST(i))
  (* nilExp: takes nothing and returns a nil constant
      (which we will treat as 0) *)
  fun nilExp() = Ex(Tr.CONST(0))
  (* stringExp: takes in a string constant, makes a label for it
      then turns it into frame data *)
  fun stringExp str =
    let
      val label = T.newlabel()
    in
      (* add this string to the list of fragments *)
      frags := (!frags)@[F.DATA({lab=label, s=str})];
      (* and give this string an expression, which
          is tied to this string *)
      Ex(Tr.NAME label)
    end
  (** VARIABLE ASSIGNMENT **)
  (* assignExp: takes in a var and an expression 
      and assigns that expression to the variable*)
  fun assignExp(var, exp) =
      Nx (assign (unEx var, unEx exp))
  (** Arithmetic and other simple expressions **)
  (* binopExp: is an auxillary function which
      handles binary operators on integers
      it takes an operator [PLUS, MINUX, MUL, DIV]
      then returns an itree expression for that binary operator *)
  fun binopExp (oper, l, r) = Ex(Tr.BINOP(oper, unEx l, unEx r))
  (* relExp: is an auxillary function to handle relational operators
      it takes an operator [EQ, NE, LT, LE, GT, GE]
        and the two expressions it is comparing
        with the relational operator
      it returns a Cx, that represents the possible jumps *)
  fun relExp (oper, l, r) =
    Cx (fn (t, f) => Tr.CJUMP (Tr.TEST(oper, unEx l, unEx r), t, f))
  (* this function is the same as relExp, except since 
      strings are compared in a confusing way this
      stands in to be replaced later*)
  fun relStrExp (oper, l, r) =
    Cx (fn (t,f) => Tr.CJUMP (Tr.TEST(oper, unEx l, unEx r), t, f))
  fun pComp(oper, l, r) =
    Cx (fn (t, f) => Tr.CJUMP (Tr.TEST(oper, unEx l, unEx r), t, f))
  (* Arithmetic and Relational Operators *)
  (* one group for integers which musr be treated differently than strings *)
  fun intOpExp (A.PlusOp, l, r)   = binopExp (Tr.PLUS, l, r)
    | intOpExp (A.MinusOp, l, r)  = binopExp (Tr.MINUS, l, r)
    | intOpExp (A.TimesOp, l, r)  = binopExp (Tr.MUL, l, r)
    | intOpExp (A.DivideOp, l, r) = binopExp (Tr.DIV, l, r)
    | intOpExp (A.EqOp, l, r)     = relExp (Tr.EQ, l, r)
    | intOpExp (A.NeqOp, l, r)    = relExp (Tr.NE, l, r)
    | intOpExp (A.LtOp, l, r)     = relExp (Tr.LT, l, r)
    | intOpExp (A.LeOp, l, r)     = relExp (Tr.LE, l, r)
    | intOpExp (A.GtOp, l, r)     = relExp (Tr.GT, l, r)
    | intOpExp (A.GeOp, l, r)     = relExp (Tr.GE, l, r)
  fun stringOpExp (A.EqOp, l, r)   = relStrExp (Tr.EQ, l, r)
    | stringOpExp (A.NeqOp, l, r)  = relStrExp (Tr.NE, l, r)
    | stringOpExp (A.LtOp, l, r)   = relStrExp (Tr.LT, l, r)
    | stringOpExp (A.LeOp, l, r)   = relStrExp (Tr.LE, l, r)
    | stringOpExp (A.GtOp, l, r)   = relStrExp (Tr.GT, l, r)
    | stringOpExp (A.GeOp, l, r)   = relStrExp (Tr.GE, l, r)      
    | stringOpExp (_, _, _)        = Er.impossible "illegal string operation: should have been caught by type checker"
  fun pOpExp(A.EqOp, l, r) = pComp(Tr.EQ, l, r)
    | pOpExp(A.NeqOp, l, r) = pComp(Tr.NE, l, r)
    | pOpExp(_, _, _) = Er.impossible "illegal pointer comparison"
  (* FUNCTION CALLS expressions:
      takes the function level
      the label for the function
      and a list of parameters *)
  fun callExp (_:level, label, exps:gexp list) =
    Ex(Tr.CALL(Tr.NAME(label), map unEx exps))
  (* LET EXPRESSIONS:
      decs: a list of variable declarations for the function body
      body: transfomred into an itree expression *)
  fun letExp([], body) = body
    | letExp(decs, body) = Ex(Tr.ESEQ(seq (map unNx decs), unEx body))
  (* SEQUENCE EXPRESSIONS:
      takes a list of expressions exps
      then it translates them into a sequence of statment, expression
      pairs for the itree -- will be canonicalized and removed later *)
  fun seqExp [] = Ex(Tr.CONST 0)
    | seqExp [exp] = exp
    | seqExp (exp :: exps) =
      Ex(Tr.ESEQ(unNx exp, unEx (seqExp exps)))
  (* ifThenExp: handles if-then expressions with no
      else clause. pg 161-162 of Appel *)
  fun ifThenExp(cond, thenExp) =
    let
      val cond = unCx(cond)
      val thenLabel = T.newlabel()
      val elseLabel = T.newlabel()
      val r = T.newtemp()
    in
      case thenExp of
        Cx _ =>
          Cx (fn (t, f) =>
            seq [(cond)(thenLabel, elseLabel),
                  Tr.LABEL thenLabel,
                  (unCx thenExp) (t, f),
                  Tr.LABEL elseLabel])
        | Nx _ =>
          Nx (seq[(cond)(thenLabel, elseLabel),
                  Tr.LABEL thenLabel,
                  unNx thenExp,
                  Tr.LABEL elseLabel])
        | Ex ex =>
          Ex(Tr.ESEQ(seq[(cond)(thenLabel, elseLabel),
                          Tr.LABEL thenLabel,
                          Tr.MOVE (Tr.TEMP r, ex),
                          Tr.LABEL elseLabel],
                        Tr.TEMP r))
    end
  (* ifExp handles if-then-else expressions:
      also from 161-162 of Appel*)
  fun ifExp(cond, thenExp, elseExp) =
    let
      val cond = unCx(cond)
      val thenLabel = T.newlabel()
      val elseLabel = T.newlabel()
      val joinLabel = T.newlabel()
      (* page 162, Appel*)
      val r = T.newtemp()
    in
      (* Appel says to tret different values for expressions differently *)
      case (thenExp, elseExp) of
        (* Appel says how handles these exps on page 162, with temporary
            Basically store whichever section will get evaluated into r
            then at the end (the join) run r as it will contain what
            is desired to evaluate. This leads to a cleaner structure *)
        (Ex _, Ex _) =>
          Ex(Tr.ESEQ (seq[(cond)(thenLabel, elseLabel),
                          Tr.LABEL thenLabel,
                          assign(Tr.TEMP r, unEx thenExp),
                          Tr.JUMP (Tr.NAME joinLabel, [joinLabel]),
                          Tr.LABEL elseLabel,
                          assign(Tr.TEMP r, unEx elseExp),
                          Tr.LABEL joinLabel],
                        Tr.TEMP r))
      (* Handling Statements is simpler than handling expressions
          because statements do not return results, therefore
          there is no need for the temporary registers that appel
          uses for tree expressions *)
      | (Nx _, Nx _) =>
          Nx (seq[(cond)(thenLabel, elseLabel),
                  Tr.LABEL thenLabel,
                  unNx thenExp,
                  Tr.JUMP (Tr.NAME joinLabel, [joinLabel]),
                  Tr.LABEL elseLabel,
                  unNx elseExp,
                  Tr.LABEL joinLabel])
      (* Handling Conditional Statements just requires
          turning this if-then-else statement into a conditional
          statement, that takes two labels and then jumps to
          the appropriate one *)
      | (Cx _, Cx _) =>
          Cx(fn (t, f) =>
              seq [(cond)(thenLabel, elseLabel),
                   Tr.LABEL thenLabel,
                   (unCx thenExp)(t, f),
                   Tr.LABEL elseLabel,
                   (unCx elseExp)(t, f)])
      | (_, _) => Er.impossible "body of if-else statement must have same type, type checker failed"
    end 
  (* while expressions: as discussed in lecture and in appel 165
    test:
      if not(condition) goto done
      body
      goto test
    done: *)
  fun whileExp(test, body) =
    let
      (* turn the test into a conditional *)
      val test = unCx test
      (* turn the body into a statement *)
      val body = unNx body
      (* add a label to the test and body *)
      val testLabel = T.newlabel()
      val bodyLabel = T.newlabel()
      val done = !breakLabel
      (* make a statement *)
      in
        Nx (
        (* if the test, then go to the body
           else jump to done *)
        (* first make the label for the condition *)
        seq[Tr.LABEL testLabel,
          (* check the condition *)
          test (bodyLabel, done),
          (* have the body label *)
          Tr.LABEL bodyLabel,
          (* run the body *)
          body,
          (* then make the loop jump back to test *)
          Tr.JUMP(Tr.NAME testLabel, [testLabel]),
          (* then have the done label, only if test failed *)
          Tr.LABEL done])
    end 
  (* for expressions: pg 166
        added in extra test lo <= hi before the increment
        this will avoid the overflow issue
        let var i := lo
          var limit := hi
        in while i <= limit
          do (body; i := i+1)
        end *)
  fun forExp(var, lo, hi, body) =
    let
      (* var, lo, and hi are all expressions *)
      val var  = unEx var
      val lo   = unEx lo
      val hi   = unEx hi
      (* body is a statement *)
      val body = unNx body
      (* make the labels for jumping *)
      val bodyLabel = T.newlabel()
      val forLabel = T.newlabel()
      val done = !breakLabel
    in
      Nx(seq[assign(var, lo), (* assign lo to var *)
          (* test the initial condition, with less than or equal
              as opposed to strictly less than *)
          Tr.CJUMP(Tr.TEST(Tr.LE, var, hi), bodyLabel, done),
          (* then have the body *)
          Tr.LABEL bodyLabel,
          body,
          (* check the condition again before jumping to test
            this will protect us against the limit=maxint case *)
          Tr.CJUMP(Tr.TEST(Tr.LT, var, hi), forLabel, done),
          (* the for loop label: dont iterate till end of first iteration *)
          Tr.LABEL forLabel,
          (* add one to var to continue on the iteration *)
          assign(var, Tr.BINOP(Tr.PLUS, var, Tr.CONST 1)),
          (* Test the forLabel again *)
          Tr.JUMP(Tr.NAME bodyLabel, [bodyLabel]),
          (* terminate the for loop *)
          Tr.LABEL done])
    end
  (* breakExp: handles break expressions for the itree
        break expressions are statements that cause us to
        jump out of the current loop
        takes a label, for where it should break to
        and returns the expression that causes that break *)
  fun breakExp() = Nx (Tr.JUMP (Tr.NAME (!breakLabel), [(!breakLabel)]))
  (* traceStack takes in a definition level (for a function)
      then takes the current level and provides a trace.
      Specifically it traces cur, the current level,
      up to the definition level, providing the sequence
      of memory moves to get there
      This is the trace static links that appel describes on pg 156
      Notes: F.wordsize is currently just guessed at *)
  fun traceStack (def, cur as LEVEL({frame=f, sl_offset=off, parent=p}, _)) =
    if leveleq(def, cur) then
      (* FP: is the base frame pointer *)
      Tr.TEMP(F.FP)
    else
      (* The size of the frame is determined by the number of locals
          for that reason we jump up by that amount until we reach the level
          that our definition is located at.
          MEM takes a size --> add in wordsize? *)
      Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.CONST (!(#locals f)), traceStack(def, p)), F.wordsize)
    (* If we have reached the top level, then this must be where it is. *)
    | traceStack (_, TOP) = Tr.TEMP(F.FP)
  (* takes access and level, and returns the expression
      that represents the given variable
      access is a (level * int), therefore we can compare the levels *)
  fun simpleVar(acc as (TOP, _), l) =
      (Er.impossible "can't declare global variable")
    | simpleVar((al, aoff), l) =
      (* find the level then offset *)
      Ex(Tr.MEM(Tr.BINOP(Tr.PLUS, traceStack(al, l), Tr.CONST(aoff)), F.wordsize))
  fun arrayVar(sz :gexp, init :gexp) =
    let
      val size = Tr.BINOP(Tr.PLUS, unEx sz, Tr.CONST(1))
      val arr = F.externalCall("initArray", [size, unEx init])
      val address = T.newtemp()
      val get_address = assign(Tr.TEMP address, arr)
    in
      (* place the size in the 0th element *)
      Ex(
      Tr.ESEQ(
        seq[
          get_address,
          assign(Tr.MEM(Tr.TEMP address, F.wordsize), unEx sz)
        ],
        Tr.TEMP address))
    end
    (* store array size as first element *)
  (* recordVarInit: is responsible for iniitializing the record space
    it goes through two phases. First it allocates the space for the
    entire record. Then it puts the initialized values into it. *)
  fun recordVarInit(r, item, exp::[]) =
      assign(
        Tr.MEM(Tr.BINOP(
          Tr.PLUS, r, unEx (intExp(item * F.wordsize))),
        F.wordsize),
      unEx exp)
    | recordVarInit(r, item, exp::exps :gexp list) =
      let
        val e = unEx exp
      in
        Tr.SEQ(
          assign(
            Tr.MEM(Tr.BINOP(Tr.PLUS, r, unEx (intExp(item * F.wordsize))), F.wordsize),
            e),
          recordVarInit(r, item+1, exps))
      end
    | recordVarInit(r, item, []) = Tr.EXP(Tr.CONST(0))
  fun recordExp(exps) =
    let
      (* allocRecord takes the number of bytes, not the length of the list *)
      val sz = List.length(exps) * F.wordsize 
      (* record pointer *)
      val alloc = F.externalCall("allocRecord", [Tr.CONST(sz)])
      val r = Tr.TEMP(T.newtemp())
      val ralloc = assign(r, alloc)
      val init = recordVarInit(r, 0, exps)
    in
      Ex(Tr.ESEQ(Tr.SEQ(ralloc, init), r))
    end
  (* subscripting and field selection:
      page 157 *)
  fun subVar(arr :gexp, offset :gexp) =
    let
      (* add is the address where this array element will be accessed *)
      val add = Temp.newtemp()
      (* arr is the array element itself *)
      val arr = unEx arr
      (* off is the offset within the array element
          the 0th element holds the size for array error checking *)
      val off = (Tr.BINOP(Tr.PLUS, unEx offset, Tr.CONST(1)))
      val sz = Tr.MEM(arr, F.wordsize)
      val offTemp = Temp.newtemp()
      val continue = Temp.newlabel()
    in
      (* Make a new label where we temporarily
        Add in each array offset * wordsize (how much do we jump over)
        Add in execution to access this new label that holds
          the information from the array *)
      (* if the offset is greater than or equal to the size *)
      Nx(
        seq[
          (* save the calculated offset into offTemp *)
          Tr.MOVE(Tr.TEMP offTemp, off),
          (* if there is an indexOutOfBounds Error, escape with error *)
          Tr.CJUMP(Tr.TEST(Tr.GT, Tr.TEMP offTemp, sz), indexOutOfBounds, continue),
          (* otherwise continue with normal behavior *)
          Tr.LABEL continue,
          unNx (Ex(Tr.ESEQ(
            assign(Tr.TEMP(add),
              Tr.BINOP(Tr.PLUS,
                arr,
                Tr.BINOP(Tr.MUL,
                  Tr.TEMP offTemp,
                  Tr.CONST(F.wordsize)))),
            Tr.MEM(Tr.TEMP(add), F.wordsize))))
          ])
    end
  (* pretty much the same as subscript var currently
      need to also add in support in semant.sml *)
  fun fieldVar(var, offset) =
    let
      val add = Temp.newtemp()
      val v = unEx var
      (* off: the offset within this record element *)
    in
      (* access the memory location of the field in record *)
      Ex(Tr.ESEQ(
          Tr.MOVE(Tr.TEMP(add),
            Tr.BINOP(Tr.PLUS,
              v,
              Tr.CONST(offset * F.wordsize))),
          Tr.MEM(Tr.TEMP(add), F.wordsize)))
    end
  fun funLabel(label, body) =
    Nx(seq[unNx (Ex(Tr.NAME(label))), unNx body])
  (* procEntryExit: described at the end of chaper 7
      it is used to gather up the fragments that have
      been generated into a frame procedure. *)
  fun procEntryExit({level=level, body=exp, label=label}) =
    let
      (* get the frame associated with this level *)
      val frame =
        (case level of
            LEVEL({frame, sl_offset, parent}, _) => frame
          | TOP => Er.impossible "TOP FRAME : Proc")

      (*val body'' = F.procEntryExit1(frame, T.MOVE(T.TEMP F.RV, unEx body))*)
      (* handles allocation of the frame *)
      val body' = F.procEntryExit1(frame, unNx(exp))
      (*val _ = print "Proc Entry Exit"
      val _ = printtree(exp)*)
      (* Create a new label for the frame (fragment) *)
      (*val label = T.newlabel()*)
      val body' = Tr.SEQ(Tr.LABEL label, body')
      (*val _ = print ("F.PROC: New Label: " ^ Symbol.name label ^ "\n")*)
      (*val after = Tr.MOVE(Tr.TEMP(F.RV), unEx(exp))*)
      val frag = F.PROC({name=label, body=body', frame=frame})
      (* add this to the end of our list of fragements *)
      val _ = (frags := (!frags)@[frag])
    in
      ()
    end
  (* mainErrors: Takes the body of a function and returns
    the body with indexOutOfBound Errors having been handled *)
  fun mainErrors(body) =
    let
      val done = T.newlabel()
      val result = T.newtemp()
    in
      Ex(Tr.ESEQ(seq[
        (* save the result we recieved *)
        Tr.MOVE(Tr.TEMP result, unEx body),
        (* jump to the done label on normal behavior *)
        Tr.JUMP(Tr.NAME done, [done]),
        (* Runtime Exceptions *)
        Tr.LABEL indexOutOfBounds,
        Tr.EXP (F.externalCall("print", [unEx (stringExp "Index Out Of Bounds Error")])),
        Tr.MOVE(Tr.TEMP result, Tr.CONST 1),
        (*T.EXP (F.externalCall("exit", [T.CONST 1])),*)
        Tr.LABEL done],
      Tr.TEMP result))
    end
  (* announce the beginnning of a function *)
    (* a label definition for the function name *)    
    (* adjust the stack pointer (allocate new frame) *)
    (* save escapint arguments (including static link) into frame
        move nonescaping arguments itno fresh temporary refisters *)
    (* store instructions to any callee-save registers -
      including return address register *)   
    (* move the return value (the result of the function) to the register
     reserved for that purpose *)
    (* load instructions to restore callee save registers *)
    (* reset the stack pointer: deallocate frame *)
    (* return address (JUMP to the return address) *)
    



end (* functor TranslateGen *)





     
