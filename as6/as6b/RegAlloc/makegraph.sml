(* makegraph.sml *)

signature MAKEGRAPH = 
sig

val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list

(* These two are for testing*)
val test : unit -> Flow.flowgraph * Flow.Graph.node list
val dump_temp_table : (Temp.temp list) Graph.Table.table * Flow.Graph.node list -> unit

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
fun instrs2graph_helper(Assem.OPER ({assem=assem, dst=dst, src=src, jump=NONE})::tail, 
	                    flow_graph as Flow.FGRAPH({control, def, use, ismove}), 
	                    nodes, instr_num, label_map) 
      = let
      		val node = List.nth(nodes, instr_num)
      		val new_def = Graph.Table.enter(def, node, dst);
      		val new_use = Graph.Table.enter(use, node, src);
      		val new_ismove = Graph.Table.enter(ismove, node, false);
      	in
      		(* Only want to make an edge to a successor if it has a successor *)
      	   (if null(tail)
      	   	then ()
      	   	else Graph.mk_edge({from=node, to=List.nth(nodes, instr_num + 1)});
        	instrs2graph_helper(tail, Flow.FGRAPH({control = control, def = new_def, use = new_use, ismove = new_ismove}), 
        		                nodes, instr_num + 1, label_map))
        end

    (* The only case whwere we do not fall through! *)
  | instrs2graph_helper(Assem.OPER ({assem=assem, dst=dst, src=src, jump=SOME(lst)})::tail, 
	                    flow_graph as Flow.FGRAPH({control, def, use, ismove}), 
	                    nodes, instr_num, label_map) 
      = let
      		val node = List.nth(nodes, instr_num);
      		val new_def = Graph.Table.enter(def, node, dst);
      		val new_use = Graph.Table.enter(use, node, src);
      		val new_ismove = Graph.Table.enter(ismove, node, false);
      		val successors = map (fn label => getOpt(Symbol.look(label_map, label), node)) lst (* TODO: Error here, should print something *)
      	in
      	   (map (fn successor => Graph.mk_edge({from=node, to=successor})) successors;
        	instrs2graph_helper(tail, Flow.FGRAPH({control = control, def = new_def, use = new_use, ismove = new_ismove}), 
        		                nodes, instr_num + 1, label_map))
      	end

      	(* TODO: I think that Label instructions should fall through *)
  | instrs2graph_helper(Assem.LABEL({assem, lab})::tail, flow_graph, nodes, instr_num, label_map)
  	  = let
  			val node = List.nth(nodes, instr_num);
  		in
  		   (if null(tail)
      	   	then ()
      	   	else Graph.mk_edge({from=node, to=List.nth(nodes, instr_num + 1)});
  			instrs2graph_helper(tail, flow_graph, nodes, instr_num + 1, label_map)) (* Labels don't really mean anything here *)
  		end

  | instrs2graph_helper(Assem.MOVE ({assem=assem, dst=dst, src=src})::tail, 
	                    flow_graph as Flow.FGRAPH({control, def, use, ismove}), 
	                    nodes, instr_num, label_map) 
      = let
      		val node = List.nth(nodes, instr_num);
      		val new_def = Graph.Table.enter(def, node, dst::nil);
      		val new_use = Graph.Table.enter(use, node, src::nil);
      		val new_ismove = Graph.Table.enter(ismove, node, true);
      	in
      	   (if null(tail)
      	   	then ()
      	   	else Graph.mk_edge({from=node, to=List.nth(nodes, instr_num + 1)});
      		instrs2graph_helper(tail, Flow.FGRAPH({control = control, def = new_def, use = new_use, ismove = new_ismove}), 
      			                nodes, instr_num + 1, label_map))
      	end

  | instrs2graph_helper(nil, flow_graph, nodes, instr_num, label_map) 
        = (flow_graph, nodes)


(* Assume all nodes have already been created and mirror the instr::is list
   Is a helper function for instrs2graph, so that we can have all the labels
   before we start trying to make the jump edges *)
fun get_labels(instr::is, node::ns, label_map) =  (case instr
	                                 				of Assem.LABEL({assem=assem, lab=lab}) => 
														let
															val label_map = Symbol.enter(label_map, lab, node)
														in
															get_labels(is, ns, label_map) 
														end
													 | _ => get_labels(is, ns, label_map))
  | get_labels(_, _, label_map) = label_map;

(* Just put an entry in each table for each node so that when we look them up later we actually find something! *)
(* ismove_table will definitely be overwritten later, def_table and use_table may or may not, and if they are not
   then that means that a particular node does not have definitions or uses, which means don't fill anything in *)
fun initialize_tables(def_table, use_table, ismove_table, node::tail) = let
																			val new_def_table = Graph.Table.enter(def_table, node, []);
																			val new_use_table = Graph.Table.enter(use_table, node, []);
																			val new_ismove_table = Graph.Table.enter(ismove_table, node, false);
																		in
																			initialize_tables(new_def_table, new_use_table, new_ismove_table, tail)
																		end
  | initialize_tables(def_table, use_table, ismove_table, nil) = (def_table, use_table, ismove_table);

(* Takes in a list of assembly instructions *)
fun instrs2graph(instrs) =  let
					  	   		val def_table = Graph.Table.empty;
								val use_table = Graph.Table.empty;
								val ismove_table = Graph.Table.empty;
								val control_graph = Graph.newGraph();
								val nodes = map (fn x => Graph.newNode(control_graph)) instrs;

								val label_map = Symbol.empty;
								val label_map = get_labels(instrs, nodes, label_map);

								val (def_table, use_table, ismove_table) = initialize_tables(def_table, use_table, ismove_table, nodes);

								val flow_graph = Flow.FGRAPH({control=control_graph, def=def_table, use=use_table, ismove=ismove_table})
					  		in
								instrs2graph_helper(instrs, flow_graph, nodes, 0, label_map) (* Start with instruction 0 *)
					  		end
(* Just for testing *)
fun dump_temp_table(temp_table, nodes) = 
	let
		val print_ints = map (fn i => print(Int.toString(i) ^ " "));
		val done = (map (fn node => (print("Node: " ^ Graph.nodename(node) ^ " => ["); 
			                         print_ints(getOpt(Graph.Table.look(temp_table, node), [~1]));
			                         print("]\n"))) nodes; (* Will be ~1 for labels or bad instructions *)
		            print (""))
	in
		done
	end

fun test() =    let
					val i1 = Assem.OPER ({assem="test", dst=[1,5,4], src=[2,1,9], jump=SOME([Symbol.symbol("a")])}); (* TODO: What happens if this list is empty? *)
					val i2 = Assem.OPER ({assem="test", dst=[0,2,3], src=[4,2,1], jump=NONE});
					val i3 = Assem.LABEL({assem="test", lab = Symbol.symbol("a")});
					val i4 = Assem.MOVE ({assem="test", dst=0, src=1});

					val (graph, nodes) = instrs2graph(i1::i2::i3::i4::nil);

				in
					(case graph
					 	of Flow.FGRAPH({control, def, use, ismove}) 
					 		=> (print("\nChecking def\n"); dump_temp_table(def, nodes); 
					 			print("\nChecking use\n"); dump_temp_table(use, nodes));
					 (graph, nodes))
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