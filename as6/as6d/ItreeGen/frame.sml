(* frame.sml *)

signature FRAME =
sig 
  type offset = int
  type frame
  (* the frame pointer *)
  val FP : Temp.temp
  val wordsize: int
  val SP : Temp.temp
  val RV : Temp.temp
  val ZERO : Temp.temp
  (* number of formals -> create a list and offset list for the formals *) 
  val newFrame : int -> frame * offset list
  (* allocInFrame : Takes a frame and allocates space, returning new offset? *)
  val allocInFrame : frame * bool -> offset

  datatype frag = PROC of {name : Temp.label, (* procedure name - is a label, a symbol *)
                           body: Tree.stm,    (* procedure body - is a statement *)
                           frame: frame}      (* procedure frame -
                                                the frame that the function is contained within *)
                (* DATA holds strings, which must be stored in a special space *)
                | DATA of {lab : Temp.label,  (* the label of a peice of data (symbol) *)
                           s   : string}      (* the string that this label describes
                                                 this is the equivalent of appels STRING *)
  val procEntryExit1 : (frame * Tree.stm) -> Tree.stm 
  val externalCall : string * Tree.exp list -> Tree.exp
end (* signature FRAME *)


structure Frame : FRAME = 
struct
  structure Tr = Tree
  
  type offset = int (* the offset within the frame *)
  
  (*** INFORMATION FOR REGISTERS ***)
  (*** BYPASSING REGALLOC ***)
  val nArgs = 4 (* number of args *)
  val nCalleSave = 18
  val nCallerSave = 0
  val FP = Register.FP (* frame pointer *)
  val SP = Register.SP (* stack pointer *)
  val RV = Register.RV (* return value *)
  val ZERO = Temp.newtemp()  
  val specialregs = [FP, SP, RV, ZERO]
  val argregs = List.tabulate (nArgs, (fn _ => Temp.newtemp()))
  val calleesaves = List.tabulate (8, (fn _ => Temp.newtemp()))
  val callersaves = List.tabulate (0, (fn _ => Temp.newtemp()))
  

  val wordsize = 4 (* typical word size is 4 bytes (an integer) *)
  (* frames are used for storing a frame *)
  type frame = {formals : int,         (* number of formal parameters *)
                offlst : offset list,  (* offset list for formals *)
                locals : int ref,      (* # of local variables so far
                                            this will be increased when
                                            new local variables are found in translation *)
                maxargs : int ref}     (* max outgoing args for the function
                                          how much this function is returning *)

  (* a new frame is allocated for every frame
      newFrame(f) provides an easy way to create frames, 
        it takes an int, describing  and returns the frame and offset *)
  fun newFrame(f) =
    ({formals=f,                                (* the new frame has the given number of formals *)
      offlst=(List.tabulate(f, fn x => x*wordsize) :offset list) ,    (* everythin is offset by wordsize? *)
      locals=ref 0,                             (* there are initially 0 local variables *)
      maxargs=ref 0},                            (* and we don't know how much we return *)
    (List.tabulate(f, fn x => 0) :offset list))


  (* allocInFrame takes a frame f and returns an offset (an int)
    BUG: No clue what the output *)
  fun allocInFrame({formals=formals, offlst=offlst, locals=locals, maxargs=maxargs}, true) =
    (* increase the number of locals by 1 *)
    (locals := !locals + 1;
    (* give back the frame offset from FP: Should be positive? *)
    (* TODO: is this offset supposed to be positive or negative *)
      (Register.paramBaseOffset + !locals * wordsize))
    | allocInFrame({formals=formals, offlst=offlst, locals=locals, maxargs=maxargs}, false) =
    (locals := !locals +1;
      (0 - (Register.localsBaseOffset + !locals * wordsize)))

  (* see function signature for specific implementation details *)
  datatype frag = PROC of {name : Temp.label, body: Tree.stm, frame: frame} 
                | DATA of {lab : Temp.label, s: string}


  (* Frame.exp: takes in an access (level, offset) pair of the desired variable
    returns a function that finds the const offset from the given frame
    fp: a frame pointer which is a label representing the frame's label *)
  fun exp(access as (level, offset)) =
    fn (fp) => Tr.MEM(Tr.BINOP(Tr.PLUS, fp, Tr.CONST(offset)), wordsize)
  (* copied from Appel *)
  fun externalCall(s, args) =
    Tr.CALL(Tr.NAME(Temp.namedlabel s), args)

  fun procEntryExit1(frame, stm) = stm

  fun procEntryExit2(frame, instructions) = ()

  fun procEntryExit3() = ()

end (* structure Frame *)

