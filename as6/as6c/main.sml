(* main.sml *)

signature MAIN = 
sig 
  val compile : string -> unit
end (* signature MAIN *)

structure Main : MAIN = 
struct
  structure Semant = SemantGen(Register)
  structure RegAlloc = RegAllocGen(Register)

  structure C = Codegen
  structure F = C.F

  (* Throw this if attach_instrs_to_temps fails *)
  exception badlists


  fun emitproc out (F.DATA {lab, s}) = TextIO.output(out,s)
                      (*** should really be output(out,C.string(lab,s)) ***)

    | emitproc out (F.PROC{name, body, frame as {formals, offlst, locals, maxargs}}) =
        let (* val _ = print ("Emit " ^ name ^ "\n") *)
            (* val _ = Printtree.printtree(out,body); *)

            val stms = Canon.linearize body
            val stms' = Canon.traceSchedule(Canon.basicBlocks stms)

            val instrs = List.concat(map C.codegen stms') 

            (* 
             * Once the RegAlloc module is ready, you can get 
             * (1) a new list of body instrs together with its live 
             *     temporaries: (Assem.instr * Temp.temp list) list
             *
             * (2) a register allocation table
             *
             * These information then can be fed into the C.procEntryExit
             * function to generate the proper function calling sequence,
             * procedure entry/exit sequences etc.
             *     
             *)
            val (flowgraph, flownodes) = MakeGraph.instrs2graph(instrs);
            val (igraph as Liveness.IGRAPH({graph, tnode, gtemp, moves}), liveness_mapping) = Liveness.interferenceGraph(flowgraph);            

            fun attach_instrs_to_temps(instr::tail1, temp::tail2) = (instr, temp)::attach_instrs_to_temps(tail1, tail2)
              | attach_instrs_to_temps(nil, nil) = nil
              | attach_instrs_to_temps(_,_) = raise badlists;

            val temps = map (fn(node) => valOf(Graph.Table.look(liveness_mapping, node))) flownodes;
            val instrs_and_temps = attach_instrs_to_temps(instrs, temp);

            val gen_formals(numformals) =
                  let
                    fun helper(index, numformals) = if index >= numformals then nil else index::helper(index + 1, numformals);
                  in
                    helper(0, numformals)
                  end

            val formals_list = gen_formals(formals)
            (*
            val procEntryExit : {name : Temp.label, 
                          body : (Assem.instr * Temp.temp list) list,
                          allocation : R.register Temp.Table.table,
                          formals : Temp.temp list,
                          frame : Frame.frame} -> Assem.instr list *)

            (* TODO: CodeGen is supposed to provide these two things *)
            val reg_list = ["reg1", "reg2", "reg3", "reg4", "reg5", "reg6", "reg7", "reg8", "reg9", "reg10", "reg11", "reg12", "reg13", "reg14", "reg15"]; (* Some sample register list *)
            val initial_allocation = Temp.Table.empty; (* Assume an empty starting table *)
            
            val color_input = {interference = igraph, initial = initial_allocation, registers = reg_list};
            val allocation = color(color_input);

            val procEntryExitInput = {name = name, body = instrs_and_temps, allocation = allocation, formals = formals_list, frame = frame}; 

            val format0 = Assem.format (fn t => "t" ^ Temp.makestring t)

         in app (fn i => TextIO.output(out,format0 i)) instrs
        end

  fun withOpenFile fname f = 
        let val out = TextIO.openOut fname
         in (f out before TextIO.closeOut out) 
               handle e => (TextIO.closeOut out; raise e)
        end 

  fun compile filename = 
        let val frags = Semant.transprog(Parse.parse filename)
         in withOpenFile (filename ^ ".s") 
                 (fn out => (app (emitproc out) frags))
        end

end (* structure Main *)


