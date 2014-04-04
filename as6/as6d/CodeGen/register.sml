(* register.sml *)

signature REGISTER =
sig 
  include REGISTER_STD

  val ECX : Temp.temp
  val EDX : Temp.temp

 (* we maintain a separate list here of true callersaves, so that
  * CodeGen will not emit code to "save" the pseudo-registers, since
  * they already live on the stack.
  *)
  val truecallersaves : register list (* CodeGen use only! *)

  (* number of pseudo-registers: *)
  val NPSEUDOREGS : int  (* CodeGen use only! *)

  (* if you like, you can add other stuff here *)
  val regname : register -> string

  (* ... *)

end (* signature REGISTER *)


structure Register : REGISTER = 
struct

  type register = string

  val RV = Temp.newtemp() (* eax is return value register *)
  val FP = Temp.newtemp() (* ebp is frame pointer *)

  val SP = Temp.newtemp() (* esp *)

  val ECX = Temp.newtemp()
  val EDX = Temp.newtemp()

  (* of course, none of the following should be empty list *)

  val NPSEUDOREGS = 8 (* change this to the proper value *)
  val localsBaseOffset : int = 0 (* change this to the proper value *)
  val paramBaseOffset : int = 4  (* this will be used to store frame pointer *)

  val specialregs : (Temp.temp * register) list = [(SP, "esp"), (FP, "ebp"),
                                                   (RV, "eax")]
  val argregs : (Temp.temp * register) list = []
  val calleesaves : register list = []
  val truecallersaves : register list = ["eax", "ecx", "edx"]
  val callersaves : register list = ["eax", "ecx", "edx"]

  (* ... other stuff ... *)

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

end (* structure Register *)

