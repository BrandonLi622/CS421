(* Brandon Li, Assignment 1 for CPSC 421 *)


(* Questions: how do we get interp to return unit? Also, when should assign statements take effect? 
   Also how to get the PATH to be persistent. Also how to raise exceptions with more complex types 
   Let statements across multiple pattern matches. Should they have a space at the end? *)

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
  | interpExp(BINOP(a,PLUS,c), t) = let val aInterped = interpExp(a,t); val cInterped = interpExp(c, #2(aInterped)) in (#1(aInterped) + #1(cInterped), #2(cInterped)) end               
  | interpExp(BINOP(a,MINUS,c), t) = let val aInterped = interpExp(a,t); val cInterped = interpExp(c, #2(aInterped)) in (#1(aInterped) - #1(cInterped), #2(cInterped)) end   
  | interpExp(BINOP(a,TIMES,c), t) = let val aInterped = interpExp(a,t); val cInterped = interpExp(c, #2(aInterped)) in (#1(aInterped) * #1(cInterped), #2(cInterped)) end   
  | interpExp(BINOP(a,DIV,c), t) = let val aInterped = interpExp(a,t); val cInterped = interpExp(c, #2(aInterped)) in (#1(aInterped) div #1(cInterped), #2(cInterped)) end   
  | interpExp(ESEQ(a,b), t) = interpExp(b, interpStm(a, t))
  
and interpStm(SEQ(a,b), t:table) = interpStm(b, interpStm(a, t))
  | interpStm(ASSIGN(a,b), t) = handleAssignStm(ASSIGN(a,b), t)
  | interpStm(PRINT(nil), t) = (print "\n"; t)
  | interpStm(PRINT(x::xs), t) = 
          let
            val evaluated = interpExp(x, t)
          in (if (length(xs) = 0) 
              then (print(toString(#1(evaluated))^"\n"); #2(evaluated))
              else (print(toString(#1(evaluated))^" "); interpStm(PRINT(xs), #2(evaluated)))) (* Spaces in between print arguments *)
          end                  
          
(* Inserts an assignment (var, value) into a table. Always start with new_t as an empty list *)
and updateAssignment(var:id, value:int, nil, new_t:table) = (var,value)::new_t
  | updateAssignment(id, value, (old_head:id_pair)::(old_tail:table), new_t) =
                         if (id = #1(old_head))
                         then updateAssignment(id, value, old_tail, new_t) (* get rid of old entries *)
                         else updateAssignment(id, value, old_tail, old_head::new_t)

(* Inserts the pair (a,b) into the table after evaluating b *)
and handleAssignStm(ASSIGN(a,b), t:table) = 
          let 
            val afterExp = interpExp(b,t) 
          in updateAssignment(a, #1(afterExp), #2(afterExp), nil) 
          end  (* should i just use the old table or move to the new table? *)
  | handleAssignStm(_, t) = t;
  (* Should only work on ASSIGN statements! *)



(* Tests *)
val t = [("a", 2), ("b", 3), ("c", 4)];
interpExp(CONST(1), t);
interpExp(VAR("a"), t);
val testEseq = ESEQ(ASSIGN("a", CONST(4)), VAR("a"));
interpExp(testEseq, t);

val testStm = SEQ(ASSIGN("a", CONST(3)), PRINT([VAR("a")]));
interpStm(testStm, t);
interp(testStm);

(**********************************************************************)
(* Extra Credit *)
(**********************************************************************)
open String;

type key = string
datatype 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree

val empty = LEAF

fun insert(LEAF,key,x) = TREE(LEAF,key,x,LEAF)
  | insert(TREE(l,k,y,r),key,x) =
               if k > key
                then TREE(insert(l,key,x),k,y,r)
               else if key > k
                then TREE(l,k,y,insert(r,key,x))
               else TREE(l,key,x,r);

fun member(key, LEAF) = false
  | member(key, TREE(l,k,x,r)) = if key = k then true else member(key,l) orelse member(key,r);
  
fun printTree(x : 'a tree, lst : (string, int), ) = 5;
               

