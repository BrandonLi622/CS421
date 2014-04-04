(* codegen.sml *)

signature CODEGEN =
sig
  structure F : FRAME
  structure R : REGISTER

  (* translate each canonical tree into a list of assembly instructions *)
  val codegen : Tree.stm -> Assem.instr list 

  (* converting a string fragment into the actual assembly code *)
  (* val string : Temp.label * string -> string *)

  (* procEntryExit sequence + function calling sequence tune-up 
   * + mapping pseudo-registers to memory load/store instructions 
   * and actual registers.
   * This is a post-pass, to be done after register allocation.
   *)
   val procEntryExit : {name : Temp.label, 
                          body : (Assem.instr * Temp.temp list) list,
                          allocation : R.register Temp.Table.table,
                          formals : Temp.temp list,
                          frame : Frame.frame} -> Assem.instr list


end (* signature CODEGEN *)

structure Codegen : CODEGEN =
struct

 structure T = Tree
 structure A = Assem
 structure Er = ErrorMsg
 structure F = Frame
 structure R = Register

 (*helper function to turn relop into corresponding x86 instruction*)
 fun relopToString (relop : T.relop) =
   case relop of
        T.EQ => "je"
      | T.FEQ => "je"
      | T.NE => "jne"
      | T.FNE => "jne"
      | T.LT => "jl"
      | T.FLT => "jl"
      | T.LE => "jle"
      | T.FLE => "jle"
      | T.GT => "jg"
      | T.FGT => "jg"
      | T.GE => "jge"
      | T.FGE => "jge"
      | T.ULT => "jb"
      | T.ULE => "jbe"
      | T.UGT => "ja"
      | T.UGE => "jae"

 fun codegen (stm : T.stm) =
 let
   val ilist = ref (nil : A.instr list)
   fun emit x = (ilist := (x :: !ilist))
   exception NotCanonicalized of string
   exception NotYetImplemented

   fun int2Str(i) =
     if i < 0 then "-"^Int.toString(~i) else Int.toString(i)


   fun munchStm(T.SEQ(stm1, stm2)) =
     raise NotCanonicalized "ITree is not canonicalized"
     (* function call *)
     | munchStm(T.MOVE(T.TEMP tmp, T.CALL(T.NAME f, args))) =
     (munchStm(T.EXP(T.CALL(T.NAME f, args)));
      emit (A.MOVE {assem = "mov `d0, eax\n", src=R.RV, dst=tmp}))
     | munchStm(T.MOVE(T.TEMP tmp, exp)) =
     emit (A.MOVE {assem = "mov `d0, `s0\n",
                   src = munchExp(exp),
                   dst = tmp})
     (* mov [reg], e2*)
     | munchStm(T.MOVE(T.MEM(T.TEMP(tmp), size), e2)) =
     emit (A.OPER {assem = "mov DWORD PTR [`s1], `s0\n",
                   src = [munchExp(e2), tmp],
                   dst = [],
                   jump = NONE})
     (* mov [exp+i], e2*)
     | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i), size), e2)) =
     emit (A.OPER {assem = "mov DWORD PTR [`s0+" ^ int2Str(i) ^"], `s1\n",
                   src = [munchExp(e1), munchExp(e2)],
                   dst = [],
                   jump = NONE})
     (* mov [i+exp], e2 *)
     | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1), size), e2)) =
     emit (A.OPER {assem = "mov DWORD PTR [`s0+" ^ int2Str(i) ^"], `s1\n",
                   src = [munchExp(e1), munchExp(e2)],
                   dst = [],
                   jump = NONE})
     (* mov [exp-i], e2 *)
     | munchStm(T.MOVE(T.MEM(T.BINOP(T.MINUS, e1, T.CONST i), size), e2)) =
     emit (A.OPER {assem = "mov DWORD PTR [`s0-" ^ int2Str(i) ^"], `s1\n",
                   src = [munchExp(e1), munchExp(e2)],
                   dst = [],
                   jump = NONE})
     (* mov [i-exp], e2 *)
     | munchStm(T.MOVE(T.MEM(T.BINOP(T.MINUS, T.CONST i, e1), size), e2)) =
     emit (A.OPER {assem = "mov DWORD PTR [" ^ int2Str(i) ^"-`s0], `s1\n",
                   src = [munchExp(e1), munchExp(e2)],
                   dst = [],
                   jump = NONE})
     (* memory to memory movement
      * need to generate the following
      * mov temp, [e2]
      * mov [e1], temp
      *)
     | munchStm(T.MOVE(T.MEM(e1, size1), T.MEM(e2, size2))) =
     let val temp = Temp.newtemp() in
     (emit (A.MOVE {assem = "mov `d0, [`s1]\n",
                   src = munchExp(e2),
                   dst = temp});
      emit (A.OPER {assem = "mov DWORD PTR [`s0], `s1\n",
                   src = [munchExp(e1), temp],
                   dst = [],
                   jump = NONE}))
     end
     | munchStm(T.JUMP(T.NAME lab, labList)) =
     emit (A.OPER {assem = "jmp " ^ Symbol.name lab ^ "\n",
            dst=[], src = [], jump=SOME(labList)})
     | munchStm(T.CJUMP(test as T.TEST(relop, exp1, exp2), lab1, lab2)) =
     (
      emit (A.OPER {assem = "cmp `s0, `s1\n", dst=[],
                    src=[munchExp(exp1), munchExp(exp2)], jump = NONE});
      emit (A.OPER {assem =
                    (relopToString relop) ^ " " ^ (Symbol.name lab1) ^ "\n",
                    src = [], dst = [], jump = SOME([lab1, lab2])})
      )
     | munchStm(T.LABEL(lab)) =
     emit (A.LABEL {assem=(Symbol.name lab) ^ ":\n", lab=lab})
     (* procedure call *)
     | munchStm(T.EXP(T.CALL(T.NAME f, args))) =
     let
       val calldefs = [R.RV, R.ECX, R.EDX]
     in
       List.app munchArg (List.rev args);
       emit (A.OPER {assem = "call " ^ (Symbol.name f) ^ "\n",
             src=[], dst = calldefs, jump = NONE});
       if (List.length args > 0) then
         emit (A.OPER {assem = "add esp, " ^
                       int2Str(List.length args * 4) ^ "\n",
                       src = [], dst=[R.SP], jump = NONE})
       else
         ();
       ()
     end
     | munchStm(T.EXP(exp)) =
     let val _ = munchExp(exp) in
       ()
     end
     | munchStm(T.MOVE(_, _)) = Er.impossible("CodeGen: illegal move pattern!")
   and result(gen) = let val t = Temp.newtemp() in gen t; t end
   and munchExp(T.ESEQ(stm1, exp1)) =
     raise NotCanonicalized "ITree is not canonicalized"
     | munchExp(T.TEMP tmp) =
     tmp
     (* load from memory address i *)
     | munchExp(T.MEM(T.CONST i, size)) =
     result (fn r => emit (A.OPER {assem = "mov `d0, [" ^ int2Str(i) ^ "]\n",
                                   src = [],
                                   dst = [r],
                                   jump = NONE}))
     (* load from [exp] *)
     | munchExp(T.MEM(exp, size)) =
     result (fn r => emit (A.MOVE {assem = "mov `d0, [`s0]\n",
                                   src = munchExp(exp),
                                   dst = r}))
     | munchExp(T.BINOP(T.MUL, exp1, exp2)) =
     result (fn r => (emit (A.MOVE {assem = "mov `d0, `s0\n",
                                   src = munchExp(exp1),
                                   dst = r});
                      emit (A.OPER {assem = "imul `d0, `s0\n",
                                    src = [munchExp(exp2), r],
                    (* imul would store overflow in EDX *)
                                    dst = [r, R.EDX],
                                    jump = NONE})))
     | munchExp(T.BINOP(T.DIV, exp1, exp2)) =
     result (fn r => (emit (A.MOVE {assem = "mov `d0, `s0\n",
                                    src = munchExp(exp1),
                                    dst = R.RV});
     (* cdq instruction extends EAX into EDX:EAX, a quad word
      * this is required if EDX is not manually initialized *)
                      emit (A.OPER {assem = "cdq\n",
                                    src = [],
                                    dst = [R.RV, R.EDX],
                                    jump = NONE});
     (* idiv requires the divident be in EAX, divisor in a GP register
      * it stores result in EAX, remainder in EDX *)
                      emit (A.OPER {assem = "idiv `s0\n",
                                    src = [munchExp(exp2), r, R.RV],
                                    dst = [R.RV, R.EDX],
                                    jump = NONE});
                      emit (A.MOVE {assem = "mov `d0, `s0\n",
                                    dst = r,
                                    src = R.RV})
                      ))
     (* i+exp *)
     | munchExp(T.BINOP(T.PLUS, T.CONST i, exp2)) =
     result (fn r => (emit (A.OPER {assem = "mov `d0, " ^ int2Str(i) ^ "\n",
                                   src = [],
                                   dst = [r],
                                   jump = NONE});
                      emit (A.OPER {assem = "add `d0, `s0\n",
                                    src = [munchExp(exp2), r],
                                    dst = [r],
                                    jump = NONE})))
     (* exp+i *)
     | munchExp(T.BINOP(T.PLUS, exp1, T.CONST i)) =
     result (fn r => (emit (A.MOVE {assem = "mov `d0, `s0\n",
                                   src = munchExp(exp1),
                                   dst = r});
                      emit (A.OPER {assem = "add `d0, " ^ int2Str(i) ^ "\n",
                                    src = [r],
                                    dst = [r],
                                    jump = NONE})))
     (* exp+exp *)
     | munchExp(T.BINOP(T.PLUS, exp1, exp2)) =
     result (fn r => (emit (A.MOVE {assem = "mov `d0, `s0\n",
                                   src = munchExp(exp1),
                                   dst = r});
                      emit (A.OPER {assem = "add `d0, `s0\n",
                                   src = [munchExp(exp2), r],
                                   dst = [r],
                                   jump = NONE})))
     | munchExp(T.BINOP(T.MINUS, exp1, exp2)) =
     result (fn r => (emit (A.MOVE {assem = "mov `d0, `s0\n",
                                    src = munchExp(exp1),
                                    dst = r});
                      emit (A.OPER {assem = "sub `d0, `s0\n",
                                    src = [munchExp(exp2), r],
                                    dst = [r],
                                    jump = NONE})))
     (* constant i *)
     | munchExp(T.CONST i) =
     result (fn r => emit (A.OPER {assem = "mov `d0, " ^ int2Str(i) ^"\n",
                                   src = [],
                                   dst = [r],
                                   jump = NONE}))
     (* named label *)
     | munchExp(T.NAME lab) =
     result (fn r => emit (A.OPER {assem = "lea `d0, " ^ Symbol.name lab ^ "\n",
                                   src = [],
                                   dst = [r],
                                   jump = NONE}))

    and munchArg (T.CONST i) =
      emit (A.OPER {assem = "push "^int2Str(i)^"\n", src=[], 
                   dst=[], jump=NONE})
      | munchArg (exp) =
      emit (A.OPER {assem = "push `s0\n", src = [munchExp(exp)],
                   dst=[], jump = NONE})

 in
   (munchStm stm; rev (!ilist))
 end

 (* fun string =  ... *)

  (* procEntryExit sequence + function calling sequence tune-up 
   * + mapping pseudo-registers to memory load/store instructions 
   * and actual registers.
   * This is a post-pass, to be done after register allocation.
   *)
  fun procEntryExit
    {name, body : (Assem.instr * Temp.temp list ) list, allocation, formals, frame}
    = let val instrs = map #1 body
          val labName = Symbol.name name
      in
        (Assem.LABEL {assem = (labName ^ ":\n"), lab=name}) :: instrs
      end


(************************************************************
  The following is an example implementation of mapping pseudo-registers 
  to memory load/store instructions and actual registers.  It is done
  in a single pass.  It assumes that pseudo-register names start with
  the letter "f".  It uses the actual registers ECX and EDX as temporaries
  when a pseudo-register is an operand of an instruction.

  There is a special case that this function does NOT handle, but you MUST!
  The DIV instruction has special requirements.  Its dividend must be in EAX, 
  its divisor in a general-purpose register.  It returns both the quotient,
  in EAX, and the remainder, in EDX regardless where the original divisor was! 
  So be careful that a divide instruction does not trash something useful
  in EDX, and that you retrieve the correct resulut from the divide instruction.
  ***********************************************************)


  (* regname -- produce an assembly language name for the given machine
   * register or psuedo-register.
   * psuedo-registers are mapped to an expression for psuedo-register's
   * location in stack frame.
   *)
  (* regname : R.register -> string *)
  fun regname reg =
      if (String.isPrefix "f" reg) then
	  (* it's a psuedo-register *)
        let
            val (SOME prNum) = Int.fromString (String.extract(reg,1,NONE));
            val offset = (prNum + 1) * 4
        in
            "-" ^ Int.toString(offset) ^ "[ebp]"
        end
      else
        reg


 (* genSpills -- do our "poor man's spilling".  Maps all pseudo-register
  * references to actual registers, by inserting instructions to load/store
  * the pseudo-register to/from a real register
  *)
 fun genSpills (insns,saytemp) =
     let
	  (* doload() -- given name of a source register src, and a true
	   * machine register mreg, will return a load instruction (if needed)
	   * and a machine register.
	   *)
	  (* loadit: Temp.temp * Temp.temp -> string * Temp.temp *)
	  fun loadit (src,mreg) =
	      let
		  val srcnm = (saytemp src)
	      in
		  if (String.isPrefix "f" srcnm) then
		      (* it's a fake register: *)
		      let
			  val _ = print ("loadit(): mapping pseudo-register `" ^ srcnm ^ "' to machine reg. `" ^ (saytemp mreg) ^"'\n");
			  val loadInsn = "\tmov\t" ^ (saytemp mreg) ^ ", " ^ (R.regname srcnm) ^ " # load pseudo-register\n"
		      in
			  (loadInsn, mreg)
		      end
		  else
		      (* no mapping needed *)
		      ("", src)
	      end
	  (* mapsrcs : produce a sequence of instructions to load
	   * pseudo-registers into real registers, and produce a list
	   * of sources which reflects the real registers.
	   *)
	  (* mapsrcs : Temp.temp list * Temp.temp list -> string * Temp.temp list *)
	  fun mapsrcs ([],_) = ("",[])
	    | mapsrcs (src::srcs,mreg::mregs) =
              let
                  val (loadInsn, src') = loadit(src,mreg)
                  val (loadRest, srcs') = mapsrcs(srcs,mregs)
              in
                  (loadInsn ^ loadRest, src'::srcs')
              end
	  (* findit -- like List.find, but returns SOME i, where i is index
	   * of element, if found
	   *)
          fun findit f l =
	      let
		  fun dosrch([],f,_) = NONE
		    | dosrch(el::els,f,idx) =
		      if f(el) then
			  SOME idx
		      else
			  dosrch(els,f,idx+1)
	      in
		  dosrch(l,f,0)
	      end

	  (* mapdsts -- after we have mapped sources to real machine
	   * registers, iterate through dsts.
	   * If dst is a pseudo-register then
	   *    if dst was also a src,
	   *         replace dst with machine register to which src is already
	   *         mapped
	   *    else
	   *         map dst to its own machine register (just use %ecx)
	   *    generate a store insn for dst to save result
	   *)
          (* mapdsts : Temp.temp list * Temp.temp list * Temp.temp list ->
	   *           string * Temp.temp list
	   *)
          (* N.B.!  This only works for dst of length 0 or 1 !! *)
          (* pre: length(dsts) <= 1 *)
	  fun mapdsts([],_,_) = ("",[])
	    | mapdsts(dst::dsts,srcs,newsrcs) =
	      let
		  val found = findit (fn e => e=dst) srcs
		  val dstnm = (saytemp dst)
	      in
		  if (isSome(found)) then
		      (* this dst is also a source *)
		      let
			  val idx=valOf(found)
			  val src=List.nth (srcs,idx)
			  val mreg=List.nth (newsrcs,idx)
		      in
			  if (src <> mreg) then
			      ("\tmov\t" ^ (R.regname dstnm) ^ ", `d0" ^ " # save pseudo-register\n", mreg::dsts)
			  else
			      (* no mapping *)
			      ("", dst::dsts)
		      end
		  else
		      (* this dst isn't a source, but it might be a pseudo-register *)
                      if (String.isPrefix "f" dstnm) then
                          (* it's a fake register: *)
                          (* we can safely just replace this destination with
                           * %ecx, and then write out %ecx to the pseudo-register
                           * location.  Even if %ecx was used to hold a different
                           * source pseudo-register, we won't end up clobbering
                           * it until after the source has been used...
                           *)
                          ("\tmov\t" ^ (R.regname dstnm) ^ ", `d0" ^ " # save pseudo-register\n", R.ECX::dsts)
                      else
                          (* no mapping *)
                          ("", dst::dsts)
	      end

	  fun mapInstr(A.OPER{assem=insn, dst=dsts, src=srcs, jump=jmp}) = 
	      let
		  val (loadinsns, newsrcs) = mapsrcs(srcs, [R.ECX, R.EDX]);
                      val (storeinsns, newdsts) = mapdsts(dsts, srcs, newsrcs); 
	      in
		  A.OPER{assem=loadinsns ^ insn ^ storeinsns,
			 dst=newdsts, src=newsrcs, jump=jmp}
	      end
	    | mapInstr(instr as A.LABEL _) = instr
	    | mapInstr(instr) =
	      (* we never generate these! *)
              ErrorMsg.impossible ("CodeGen: unexpected instruction type in mapInstr!")
     in
         map mapInstr insns
     end

  (***************************************************)
end (* structure Codegen *)
