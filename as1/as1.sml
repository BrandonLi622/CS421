(* Brandon Li, Assignment 1 for CPSC 421 *)

open Int;

type id = string

datatype binop = PLUS | MINUS | TIMES | DIV

datatype stm = SEQ of stm * stm
	         | ASSIGN of id * exp
	         | PRINT of exp list

     and exp = VAR of id
	         | CONST of int
             | BINOP of exp * binop * exp
             | ESEQ of stm * exp

val prog = 
SEQ(ASSIGN("a",BINOP(CONST 5, PLUS, CONST 3)),
    SEQ(ASSIGN("b",ESEQ(PRINT[VAR"a",BINOP(VAR"a",MINUS,CONST 1)],
	                    BINOP(CONST 10, TIMES, VAR"a"))),
        PRINT[VAR "b"]))

(* My own test program... adapted from book *)
val prog2 = SEQ(ASSIGN("a", BINOP(CONST(5), PLUS, CONST(3))),
                SEQ(ASSIGN("b", ESEQ(PRINT([VAR("b"), VAR("c")]),
                                     VAR("hello"))), 
                    PRINT([VAR("b")])));


(**********************************************************************)
(* Problem 1 *)
(**********************************************************************)

(* The approach that I took to this problem was to write two mutually
   recursive functions that each handled the mutually recursively data
   types stm and exp. Essentially, the computation boils down to comparing
   the length of a print statement's arguments with the maximum length of
   any nested print statements. So, the main thing to watch out for was not
   adding arguments from nested print statements, and to keep track of
   the maximum length seen even at different levels *)

fun max(a, b) = if (a > b) then a else b;

fun max_of_list(nil) = 0
  | max_of_list(a::b::xs) = max_of_list(max(a,b)::xs)
  | max_of_list(a::xs) = a;



(* Finds longest print statement and returns length for exp data types 
   Auxiliary function to maxargs because ESEQ could contain statements *)
fun exp_maxargs(ESEQ(a,b)) = max(maxargs(a), exp_maxargs(b))
  | exp_maxargs(VAR(a)) = 0
  | exp_maxargs(CONST(a)) = 0
  | exp_maxargs(BINOP(a,b,c)) = max(exp_maxargs(a), exp_maxargs(c))

(* Finds longest print statement and returns length FOR stm data types *)
and maxargs(SEQ(a,b)) = max(maxargs(a), maxargs(b))
  | maxargs(ASSIGN(a,b)) = exp_maxargs(b)
  | maxargs(PRINT(explist)) = max_of_list(length(explist)::map(exp_maxargs)(explist)); 
                             (* either the length of the list or maxargs of any individual element
                                because nested print statements should not be summed *) 
  

(**********************************************************************)
(* Problem 2 *)
(**********************************************************************)

(* For this problem, I separated out different steps such as updating the table,
   evaluating an expression, and printing into different functions. The main
   challenge was realizing that as a linear program, ASSIGN statements
   that were nested needed to affect the assignment table for all subsequent statements.
   The way that I did this is by grabbing the updated table after evaluating each
   statement and expression and passing it to the next instruction.
   *)

type id_pair = id * int
type table = id_pair list

exception badVariableReference;
exception badVariableReferenceVerbose of string;

(* From table gets value associated with a *)
fun tableLookup(a:id, (t_head:id_pair)::(t_tail:table)) = if (a = #1(t_head)) then #2(t_head) else tableLookup(a, t_tail)
  | tableLookup(a, nil) = raise badVariableReference
  
and debugTableLookup(a:id, orig_t:table) = tableLookup(a, orig_t) handle
                                          badVariableReference => raise badVariableReferenceVerbose(#1(hd(orig_t))); 



fun interp(a:stm) = (interpStm(a, nil); ())

(* Evaluates an expression based on the contents of the table
   Returns the expression's value along with the updated table *)
and interpExp(CONST(a), t:table) = (a, t)
  | interpExp(VAR(a), t) = (debugTableLookup(a, t), t)
  | interpExp(BINOP(a,oper,c), t) = let 
                                    val aInterped = interpExp(a,t); 
                                    val cInterped = interpExp(c, #2(aInterped)) 
                                  in
                                    case oper of
                                        PLUS  => (#1(aInterped) + #1(cInterped),   #2(cInterped))
                                      | MINUS => (#1(aInterped) - #1(cInterped),   #2(cInterped))
                                      | TIMES => (#1(aInterped) * #1(cInterped),   #2(cInterped))
                                      | DIV   => (#1(aInterped) div #1(cInterped), #2(cInterped))
                                  end
  | interpExp(ESEQ(a,b), t) = interpExp(b, interpStm(a, t))
  
and interpStm(SEQ(a,b), t:table) = interpStm(b, interpStm(a, t))
  | interpStm(ASSIGN(a,b), t) = handleAssignStm(ASSIGN(a,b), t)
  | interpStm(PRINT(nil), t) = (print "\n"; t)
  | interpStm(PRINT(x::xs), t) = 
          let
            val evaluated = interpExp(x, t)
          in (if (length(xs) = 0) 
              then (print(toString(#1(evaluated))^" \n"); #2(evaluated))
              else (print(toString(#1(evaluated))^" "); interpStm(PRINT(xs), #2(evaluated)))) 
              (* Spaces in between print arguments *)
          end                  
          
(* Inserts an assignment (var, value) into a table. Always start with new_t as an empty list *)
and updateAssignment(var:id, value:int, nil, new_t:table) = (var,value)::new_t
  | updateAssignment(id, value, (old_head:id_pair)::(old_tail:table), new_t) =
                         if (id = #1(old_head))
                         then updateAssignment(id, value, old_tail, new_t) (* get rid of to-be-replaced entry *)
                         else updateAssignment(id, value, old_tail, old_head::new_t)

(* Inserts the pair (a,b) into the table after evaluating b *)
and handleAssignStm(ASSIGN(a,b), t:table) = 
          let 
            val afterExp = interpExp(b,t) 
          in updateAssignment(a, #1(afterExp), #2(afterExp), nil) 
          end 
  | handleAssignStm(_, t) = t;
  (* Should only work on ASSIGN statements! *)


(*
(* Tests *)
val t = [("a", 2), ("b", 3), ("c", 4)];
interpExp(CONST(1), t);
interpExp(VAR("a"), t);
val testEseq = ESEQ(ASSIGN("a", CONST(4)), VAR("a"));
interpExp(testEseq, t);

val testStm = SEQ(ASSIGN("a", CONST(3)), PRINT([VAR("a")]));
interpStm(testStm, t);
interp(testStm);
*)
