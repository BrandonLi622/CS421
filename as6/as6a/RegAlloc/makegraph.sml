(* makegraph.sml *)

signature MAKEGRAPH = 
sig

val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list

end

structure MakeGraph : MAKEGRAPH =
struct

(* The "instrs2graph" function takes a list of assembly instructions,
   and constructs its flowgraph and also returns the list of nodes in 
   the flowgraph. The instructions exactly correspond to the nodes in 
   the graph. If instruction m can be followed by instruction n (either
   by a jump or by falling through), there should be an edge from m to n
   in the graph.

   The flowgraph also maintains several attributes for each node in the 
   graph, i.e., the "def" set, the "use" set, and the "ismove" flag

 *)


(* A helper function that iterates over instructions and adds to the different fields and finally returns the appropriate tuple, which
	is a pair of (flowgraph, nodes) *)
fun instrs2graph_helper(instr::tail, instrNum, flowGraph, nodes) =  (case instr
 																							of Assem.OPER ({assem=assem, dst=dst, src=src, jump=NONE}) => 0
 																							 | Assem.OPER ({assem=assem, dst=dst, src=src, jump=SOME(lst)}) => 0
 																							 | Assem.LABEL({assem=assem, lab=lab}) => 0
 																							 | Assem.MOVE ({assem=assem, dst=dst, src=src}) => 0;



	                                                                                    instrs2graph_helper(tail, instrNum + 1, flowGraph, nodes))
  | instrs2graph_helper(nil, _, flowGraph, nodes) = (flowGraph, nodes)

(*
fun getLabels(instrs) = let
							val labelMap = Symbol.Table.empty;
							val labelMap labelMap.enter(labelMap, , ) *)

(* Takes in a list of assembly instructions *)
fun instrs2graph(instrs) =  let
					  	   		val defTable = Graph.Table.empty;
								val useTable = Graph.Table.empty;
								val ismoveTable = Graph.Table.empty;
								val controlGraph = Graph.newGraph();
								val nodes = map (fn x => Graph.newNode(controlGraph)) instrs;

								val flowGraph = Flow.FGRAPH({control=controlGraph, def=defTable, use=useTable, ismove=ismoveTable})
					  		in
								instrs2graph_helper(instrs, 0, flowGraph, nodes)
					  		end

(* (* Just the part a *)
(* Takes in a list of assembly instructions *)
fun instrs2graph(x) = let
					  	val defTable = Graph.Table.empty;
						val useTable = Graph.Table.empty;
						val ismoveTable = Graph.Table.empty;
						val fGraph = Graph.newGraph()

						val n1 = Graph.newNode(fGraph); (* The node representing the instruction: a := 0 *)
						val n2 = Graph.newNode(fGraph); (* The node representing the instruction: b := a + 1 *)
						val n3 = Graph.newNode(fGraph); (* The node representing the instruction: c := c + b *)
						val n4 = Graph.newNode(fGraph); (* The node representing the instruction: a := b * 2 *)
                         
						(* Since Temp.temp, which represents a register, must be of type int, let's say that
					  		a is represented by 1, b is represented by 2, c is represented by 3, and d is
					  		represented by 4*)
						val defTable = Graph.Table.enter(defTable, n1, 1::nil);
					  	val defTable = Graph.Table.enter(defTable, n2, 2::nil);
					  	val defTable = Graph.Table.enter(defTable, n3, 3::nil);
					  	val defTable = Graph.Table.enter(defTable, n4, 1::nil);

					  	val useTable = Graph.Table.enter(useTable, n1, nil);
					  	val useTable = Graph.Table.enter(useTable, n2, 1::nil);
					  	val useTable = Graph.Table.enter(useTable, n3, 2::3::nil);
					  	val useTable = Graph.Table.enter(useTable, n4, 2::nil);

					  	(* TODO: What counts as a move exactly? *)
					  	val ismoveTable = Graph.Table.enter(ismoveTable, n1, false);
					  	val ismoveTable = Graph.Table.enter(ismoveTable, n2, false);
					  	val ismoveTable = Graph.Table.enter(ismoveTable, n3, false);
					  	val ismoveTable = Graph.Table.enter(ismoveTable, n4, false); 

					  in
						Graph.mk_edge({from=n1, to=n2});
						Graph.mk_edge({from=n2, to=n3});
						Graph.mk_edge({from=n3, to=n4});

					  	(* Return value is the graph plus the list of nodes *)
					  	(Flow.FGRAPH({control=fGraph, def=defTable, use=useTable, ismove=ismoveTable}), n1::n2::n3::n4::nil)
					  end *)
end