(* regalloc.sml *)

signature REG_ALLOC =
sig
   structure R : REGISTER_STD
   
   type allocation = Register.register Temp.Table.table

   val color : {interference : Liveness.igraph,
                initial : allocation,
                registers : R.register list} -> allocation

end (* signature REG_ALLOC *)

functor RegAllocGen(Register : REGISTER_STD) : REG_ALLOC =
struct
   structure R : REGISTER_STD = Register

   type allocation = R.register Temp.Table.table

   (* The color function talkes an initial allocation table (which assigns
      temporary variables such as FP or SP into certain fixed registers)
      plus an interference graph and a list of registers, and returns
      a new allocation table (mapping from temporaries to registers).

      Notice, you don't need to implement spilling and coalescing. 
      Just do the "simplify" and then do the "select".
    *)

   fun color {interference, initial, registers} =  (*initial*)  let
                                                                  val alloc = Temp.Table.empty;
                                                                  val alloc = Temp.Table.enter(alloc, 1, "reg1"); (* temp a (number 1) goes into register 1 *)
                                                                  val alloc = Temp.Table.enter(alloc, 2, "reg1"); (* temp b (number 2) goes into register 1 *)
                                                                  val alloc = Temp.Table.enter(alloc, 3, "reg2"); (* temp c (number 3) goes into register 2 *)
                                                                in
                                                                  alloc
                                                                end;

end (* functor RegAllocGen *)
