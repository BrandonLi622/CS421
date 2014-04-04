(* liveness.sml *)

signature LIVENESS =
sig
  (* This is different from the book, but I suppose that's okay... *)
  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table,
                 gtemp : Temp.temp Graph.Table.table,
                 moves : (Graph.node * Graph.node) list} (* I will have the src be first and the dst be second *)

  
     val interferenceGraph : 
           Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list) 
   
   (*  val show : outstream * igraph -> unit
   *)

(* Just for testing *)
val sort_list : int list -> int list
val to_sorted_set : int list -> int list
val set_difference : int list * int list -> int list
val set_union : int list * int list -> int list

val test : unit -> unit


end (* signature LIVENESS *)

structure Liveness : LIVENESS = 
struct

  (* To construct the interference graph, it is convenient to
     construct a liveness map at each node in the FlowGraph first.
     For each node in the flowgraph, i.e., for each assembly 
     instruction, we want to easily look up the set S of live 
     temporaries. 
   *)
  (* 
  type liveSet = unit Temp.Table.table * Temp.temp list
  type livenessMap = liveSet Flow.Graph.Table.table 
  *)

  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table,
                 gtemp : Temp.temp Graph.Table.table,
                 moves : (Graph.node * Graph.node) list}

  (********************************************************************************************************************************************)
  (****************************************************        Set Utilities        ***********************************************************)
  (********************************************************************************************************************************************)

  (* Right now is actually just insertion sort *)
  fun sort_list(lis) =  let
                          fun insert(n, h::t) = if n < h then n::h::t else h::insert(n, t)
                            | insert(n, []) = [n]
                        in
                          foldl insert [] lis
                        end;

  (* Does all of the hard work for to_sorted_set *)
  fun to_sorted_set_helper(a::b::xs : int list) = if a = b then to_sorted_set_helper(a::xs) else a::to_sorted_set_helper(b::xs)
    | to_sorted_set_helper(a::nil) = a::nil
    | to_sorted_set_helper(nil) = nil; 

  (* Removes all duplicates from and sorts @lis *)
  fun to_sorted_set(lis) = to_sorted_set_helper(sort_list(lis));

  fun set_union(set1, set2) = to_sorted_set(set1@set2);

  fun set_difference(set1 as a::xs : int list, set2 : int list) = if List.exists (fn b => if a = b then true else false) set2
                                                                  then set_difference(xs, set2)
                                                                  else a::set_difference(xs, set2)
    | set_difference(nil, _) = nil;

  (* For two sorted sets, checks if they are equal *)
  (* NOTE: Assumes that they are indeed sorted sets! *)
  fun sets_are_equal_helper(a::sorted1 : int list, b::sorted2 : int list) = if a <> b then false else sets_are_equal_helper(sorted1, sorted2)
    | sets_are_equal_helper(nil, nil) = true
    | sets_are_equal_helper(_, _) = false (* This means they were of different lengths... *)

  (* Checks if two int list option's are equal *)
  fun sets_are_equal(SOME(set1), SOME(set2)) = sets_are_equal_helper(to_sorted_set(set1), to_sorted_set(set2))
    | sets_are_equal(_,_) = false (* If one of the two sets are not in the table somehow they cannot be equal *)


  (********************************************************************************************************************************************)
  (****************************************************     Graph/Table Utilites    ***********************************************************)
  (********************************************************************************************************************************************)
  
  (* Performs the first part of the algorithm on page 214, just initialize all in[n] and out[n] to empty sets *)
  (* Here, nodes are from the control flow graph*)
  fun initialize_sets(in_table, out_table, node::tail) =  let
                                                            val new_in_table = Graph.Table.enter(in_table, node, []); 
                                                            val new_out_table = Graph.Table.enter(out_table, node, [])
                                                          in
                                                            initialize_sets(new_in_table, new_out_table, tail)
                                                          end
    | initialize_sets(in_table, out_table, nil) = (in_table, out_table)

  (* Does a lookup and gets rid of the annoying option type *)
  (* @typ is strictly for debugging *)
  fun lookup_int_list(table, node, typ) =   case Graph.Table.look(table, node)
                                              of NONE => (print ("Could not find node in '" ^ typ ^ "' table: " ^ Graph.nodename(node) ^ "\n"); [])
                                               | SOME(i) => i;

  
  (* Converts a use or def table from makegraph into a livenessMap type *)
  (* Here, nodes are from the control flow graph, assume liveness_map already exists *)
  fun to_liveness_map(flowgraph_table, liveness_map, node::tail) = 
      let
        val temps = getOpt(Graph.Table.look(flowgraph_table, node), []);
        val new_temp_table = Temp.Table.empty;
        val new_temp_table = foldr (fn (temp, table) => Temp.Table.enter(table, temp, ())) new_temp_table temps; (* Insert all temps into table *)
        val liveness_map = Graph.Table.enter(liveness_map, node, (new_temp_table, temps))
      in
        to_liveness_map(flowgraph_table, liveness_map, tail)
      end
    | to_liveness_map(flowgraph_table, liveness_map, nil) = liveness_map;

  (* Creates the mappings for tnode and gtemp by first constructing tables and then turning them into functions *)
  fun create_igraph_mappings(temp::tail, tnode_table, gtemp_table, igraph : Graph.graph) 
        = let
            val igraph_node = Graph.newNode(igraph);
            val tnode_table = Temp.Table.enter(tnode_table, temp, igraph_node);
            val gtemp_table = Graph.Table.enter(gtemp_table, igraph_node, temp)
          in
            create_igraph_mappings(tail, tnode_table, gtemp_table, igraph)
          end
    | create_igraph_mappings(nil, tnode_table, gtemp_table, igraph) = (tnode_table, gtemp_table);


  fun are_adjacent(node1, node2) =  let
                                      val node1_adj = Graph.adj(node1);
                                      fun is_member(n1::tail, n2) = if Graph.eq(n1,n2)
                                                                    then true
                                                                    else is_member(tail,n2)
                                        | is_member(nil, n2) = false; 
                                    in
                                      is_member(node1_adj,node2)
                                    end;

  (* A helper function that makes edges between a defined temporary and all live temporaries at a given node *)
  fun one_node_to_many(def_node, live_node::tail) = (if not(Graph.eq(def_node, live_node)) andalso not(are_adjacent(def_node, live_node))
                                                     then Graph.mk_edge({from = def_node, to = live_node})
                                                     else (); (*(print("FAILED: (" ^ Graph.nodename(def_node) ^ ", " ^ Graph.nodename(live_node) ^ ")\n")); *)
                                                     one_node_to_many(def_node, tail))
    | one_node_to_many(def_node, nil) = ();

  (* A helper function to make edges between all defined temporaries and all live temporaries at a node *)
  fun many_nodes_to_many(def_node::tail, live_nodes) = (one_node_to_many(def_node, live_nodes); many_nodes_to_many(tail, live_nodes))
    | many_nodes_to_many(nil, live_nodes) = ();

  (* Returns a list of all of the temporaries that are defined *)
  (* Here, nodes are from the control flow graph *)
  fun get_all_temporaries(node::tail, def_table, current_temps) = 
        let
          val temp_list = getOpt(Graph.Table.look(def_table, node), [])
        in
          get_all_temporaries(tail, def_table, current_temps@temp_list)
        end
    | get_all_temporaries(nil, _, current_temps) = to_sorted_set(current_temps); (* List might have duplicates so we handle that here *)

  (* Use out_table and def_table to look up the edges to draw *)
  (* Note: The input parameter nodes are from the flowgraph, but we need to draw edges on the igraph *)
  (* We assume here that all of the igraph nodes are already created *)
  (* TODO: are the edges directed? Do we need to draw 2 directed edges between each node? *)
  fun make_graph_edges(igraph, node::tail, def_table, out_table, tnode_table) =
    let
      val defs = lookup_int_list(def_table, node, "DEF"); (* There should really only be one here... *)
      val err = if List.length(defs) > 1 then print("Error: Should not have more than one definition at a node\n") else ();
      val lives = lookup_int_list(out_table, node, "OUT");

      (* TODO: Figure out what to do if we have an exception *)
      fun node_lookup(temp) = valOf(Temp.Table.look(tnode_table, temp)) handle Option => (print("Cannot find temp: " ^ Int.toString(temp) ^ "\n"); 
                                                                                          map (fn(temp) => print(Int.toString(temp) ^ " ")) (get_all_temporaries(Graph.nodes(igraph), def_table, []));
                                                                                          raise Option) ;

      (* Convert the temporaries to actual nodes *)
      val defs = map (fn(temp) => node_lookup(temp)) defs;
      val lives = map (fn(live) => node_lookup(live)) lives;
    in
      (many_nodes_to_many(defs, lives);
       make_graph_edges(igraph, tail, def_table, out_table, tnode_table))
      (*map (fn(def) => (map (fn(live) => Graph.mk_edge({from = def, to = live})) lives)) defs*)
    end
    | make_graph_edges(_,_,_,_,_) = ();

  (********************************************************************************************************************************************)
  (****************************************************       Other Utilities       ***********************************************************)
  (********************************************************************************************************************************************)

  (* Returns a (node,node) list of all of the moves *)
  (* @node is a node from the control flow graph, but this will return a list of pairs of igraph nodes! *)
  (* @def, @use, and @ismove are from flowgraph *)
  (* TODO: Not implemented yet, but also not super critical yet! *)
  fun create_moves_list(temps : Temp.temp list, tnode, def, use, ismove, node::tail) = nil
    | create_moves_list(temps, tnode, def, use, ismove, nil) = nil;

    
  (********************************************************************************************************************************************)
  (****************************************************      Forward Algorithm      ***********************************************************)
  (********************************************************************************************************************************************)

  (* A helper function for get_liveness_edges, outlined on page 214 *)
  (* If for every node, the condition for termination in[n] = in'[n] and out[n] = out'[n] checks, then true *)
  (* Here, nodes are from the control flow graph*)
  fun check_termination(node::tail, in_table, in'_table, out_table, out'_table) = 
        let
          (* These are all option types *)
          val in_i = Graph.Table.look(in_table, node);
          val in'_i = Graph.Table.look(in'_table, node);
          val out_i = Graph.Table.look(out_table, node);
          val out'_i = Graph.Table.look(out'_table, node)
        in
          (* If the sets are not equal, then the condition on page 214 fails *)
          if (sets_are_equal(in_i, in'_i) andalso sets_are_equal(out_i, out'_i))
          then check_termination(tail, in_table, in'_table, out_table, out'_table)
          else false
        end
    | check_termination(nil, _,_,_,_) = true

  (* For each node does the innermost loop as shown on page 214 *)
  (* Here, nodes are from the control flow graph*)
  fun iterate(node::tail,
              in_table, in'_table,
              out_table, out'_table,
              def_table, use_table)
        = let
            val in'_table = Graph.Table.enter(in'_table, node, lookup_int_list(in_table, node, "IN"));
            val out'_table = Graph.Table.enter(out'_table, node, lookup_int_list(out_table, node, "OUT"));

            val temp1 = set_difference(lookup_int_list(out_table, node, "OUT"), lookup_int_list(def_table, node, "DEF"))
            val temp2 = set_union(lookup_int_list(use_table, node, "USE"), temp1);
            val in_table = Graph.Table.enter(in_table, node, temp2);

            val successors = Graph.succ(node);
            val temp = foldl (fn(node, int_list) => set_union(lookup_int_list(in_table, node, "IN"), int_list)) [] successors;
            val out_table = Graph.Table.enter(out_table, node, temp);
          in
            iterate(tail, in_table, in'_table, out_table, out'_table, def_table, use_table)
          end
    | iterate(nil,
              in_table, in'_table,
              out_table, out'_table,
              def_table, use_table)
        = (in_table, in'_table, out_table, out'_table)

  (* Assume in[n] and out[n] are already all initialized, performs the repeat...until loop on page 214 
     The iteration will always be run at least once assuming false is passed in as the last argument *)
  (* Here, nodes are from the control flow graph*)
  fun get_liveness_edges (nodes, 
                          in_table, in'_table, 
                          out_table, out'_table, 
                          def_table, use_table,
                          false)
        = let
            (*TODO: Remove all of the debug stuff when done*)
            (* Add in if debugging *)
            (*
            val x = print("In Table:\n");
            val x = MakeGraph.dump_temp_table(in_table, nodes);
            val x = print("\nOut Table:\n");
            val x = MakeGraph.dump_temp_table(out_table, nodes);
            val x = print("\nIn' Table:\n");
            val x = MakeGraph.dump_temp_table(in'_table, nodes);
            val x = print("\nOut' Table:\n");
            val x = MakeGraph.dump_temp_table(out'_table, nodes);
            val x = print("\n\n") *)

            (* Iterate once to get all of the new tables *)
            val (in_table, in'_table, out_table, out'_table) = iterate(nodes, in_table, in'_table, out_table, out'_table, def_table, use_table)
          in
            (* Then check if we need to stop *)
            get_liveness_edges(nodes, in_table, in'_table, out_table, out'_table, 
                               def_table, use_table, 
                               check_termination(nodes, in_table, in'_table, out_table, out'_table))
          end                    
    | get_liveness_edges (nodes, 
                          in_table, in'_table, 
                          out_table, out'_table, 
                          def_table, use_table,
                          true)
        = let
            (*TODO: Remove all of the debug stuff when done*)
            (* Add in if debugging *)
            (*
            val x = print("In Table:\n");
            val x = MakeGraph.dump_temp_table(in_table, nodes);
            val x = print("\nOut Table:\n");
            val x = MakeGraph.dump_temp_table(out_table, nodes);
            val x = print("\nIn' Table:\n");
            val x = MakeGraph.dump_temp_table(in'_table, nodes);
            val x = print("\nOut' Table:\n");
            val x = MakeGraph.dump_temp_table(out'_table, nodes);
            val x = print("\n\n") *)
        in
          (in_table, in'_table, out_table, out'_table)
        end

  (********************************************************************************************************************************************)
  (****************************************************      Reverse Algorithm      ***********************************************************)
  (********************************************************************************************************************************************)

  (* Alternative algorithm *)
  (* Used for get_liveness_edges_rev, flips the evaluation of in and out to make the algorithm faster 
     Assumes that the nodes are passed in reverse order*)
  fun iterate_rev(node::tail,
                  in_table, in'_table,
                  out_table, out'_table,
                  def_table, use_table)
        = let
            val out'_table = Graph.Table.enter(out'_table, node, lookup_int_list(out_table, node, "OUT"));
            val in'_table = Graph.Table.enter(in'_table, node, lookup_int_list(in_table, node, "IN"));

            val successors = Graph.succ(node);
            val temp = foldl (fn(node, int_list) => set_union(lookup_int_list(in_table, node, "IN"), int_list)) [] successors;
            val out_table = Graph.Table.enter(out_table, node, temp);

            val temp1 = set_difference(lookup_int_list(out_table, node, "OUT"), lookup_int_list(def_table, node, "DEF"))
            val temp2 = set_union(lookup_int_list(use_table, node, "USE"), temp1);
            val in_table = Graph.Table.enter(in_table, node, temp2);
          in
            iterate_rev(tail, in_table, in'_table, out_table, out'_table, def_table, use_table)
          end
    | iterate_rev(nil,
                  in_table, in'_table,
                  out_table, out'_table,
                  def_table, use_table)
        = (in_table, in'_table, out_table, out'_table)

  (* Alternative algorithm *)
  (* Traverse the nodes in reverse order, and also flip evaluation of in and out *)
  fun get_liveness_edges_rev (nodes, 
                              in_table, in'_table, 
                              out_table, out'_table, 
                              def_table, use_table,
                              false)
        = let

            (*TODO: Remove all of the debug stuff when done*)
            (* Add in if debugging *)
            (*
            val x = print("In Table:\n");
            val x = MakeGraph.dump_temp_table(in_table, nodes);
            val x = print("\nOut Table:\n");
            val x = MakeGraph.dump_temp_table(out_table, nodes);
            val x = print("\nIn' Table:\n");
            val x = MakeGraph.dump_temp_table(in'_table, nodes);
            val x = print("\nOut' Table:\n");
            val x = MakeGraph.dump_temp_table(out'_table, nodes);
            val x = print("\n\n") *)

            (* Iterate once to get all of the new tables *)
            val (in_table, in'_table, out_table, out'_table) = iterate_rev(List.rev(nodes), in_table, in'_table, out_table, out'_table, def_table, use_table)
          in
            (* Then check if we need to stop *)
            get_liveness_edges(nodes, in_table, in'_table, out_table, out'_table, 
                               def_table, use_table, 
                               check_termination(nodes, in_table, in'_table, out_table, out'_table))
          end                    
    | get_liveness_edges_rev (nodes, 
                              in_table, in'_table, 
                              out_table, out'_table, 
                              def_table, use_table,
                              true)
        = let
            (*TODO: Remove all of the debug stuff when done*)
            (* Add in if debugging *)
            (*
            val x = print("In Table:\n");
            val x = MakeGraph.dump_temp_table(in_table, nodes);
            val x = print("\nOut Table:\n");
            val x = MakeGraph.dump_temp_table(out_table, nodes);
            val x = print("\nIn' Table:\n");
            val x = MakeGraph.dump_temp_table(in'_table, nodes);
            val x = print("\nOut' Table:\n");
            val x = MakeGraph.dump_temp_table(out'_table, nodes);
            val x = print("\n\n") *)
        in
          (in_table, in'_table, out_table, out'_table)
        end


  (********************************************************************************************************************************************)
  (****************************************************     Debugging Functions     ***********************************************************)
  (********************************************************************************************************************************************)

  (* Prints the temporaries *)
  fun show_temps(alltemps) = (print "Temporaries used: "; map (fn (temp) => print(Int.toString(temp) ^ " ")) alltemps; print ("\n"));

  (* Prints the edges on the control_graph *)
  fun show_edges(control_graph, node::tail) = (let val successors = Graph.succ(node) 
                                               in map (fn (succ) => print("Edge: (" ^ Graph.nodename(node) ^ ", " ^ Graph.nodename(succ) ^ ")\n")) successors 
                                               end; 
                                               show_edges(control_graph, tail))
    | show_edges(control_graph, nil) = ();

  (* Prints all of the adjacencies between nodes *)
  fun show_adj(node::tail) =  let
                                val adj_nodes = Graph.adj(node)
                              in
                                (map (fn(adj_node) => print("(" ^ Graph.nodename(node) ^ ", " ^ Graph.nodename(adj_node) ^ ")\n")) adj_nodes; show_adj(tail))
                              end
    | show_adj(nil) = ();


  (********************************************************************************************************************************************)
  (****************************************************      interferenceGraph      ***********************************************************)
  (********************************************************************************************************************************************)

  (* TODO: I'm not sure if we really need to use livenessMap... *)
  fun interferenceGraph(Flow.FGRAPH({control, def, use, ismove})) =
      let

        (* Not sure why we didn't just pass nodes in from makegraph, but this is fine too *)
        val nodes = Graph.nodes(control);

        (* A bunch of initialization stuff *)
        val igraph = Graph.newGraph();
        val tnode = Temp.Table.empty;
        val gtemp = Graph.Table.empty;
        val moves = nil; 
        val in_table = Graph.Table.empty;  (* TODO: Should I use liveness map for these instead? *)
        val in'_table = Graph.Table.empty;
        val out_table = Graph.Table.empty;
        val out'_table = Graph.Table.empty;
        val def_table = def; (* to_liveness_map(def, Graph.Table.empty, nodes) *) (* In case I want to turn it into another form *)
        val use_table = use; (* to_liveness_map(use, Graph.Table.empty, nodes) *)

        val terminate_flag = false; (* This is used later in the recursive application of the interference algorithm *)

        (* Fill in the use and def tables because they are empty, ignore the output *)
        val (in_table, out_table) = initialize_sets(in_table, out_table, nodes);
        val (in'_table, out'_table) = initialize_sets(in'_table, out'_table, nodes);

        (* Add in if debugginging *)
        val a = print("Results:\n");
        val a = print("In table:\n");
        val x = MakeGraph.dump_temp_table(in_table, nodes);
        val a = print("\nOut table:\n");
        val y = MakeGraph.dump_temp_table(out_table, nodes);
        val a = print("\nDef table:\n");
        val x = MakeGraph.dump_temp_table(def_table, nodes);
        val a = print("\nUse table:\n");
        val y = MakeGraph.dump_temp_table(use_table, nodes);
        val x = print("\n");
        

        (* Run the interference algorithm. I suppose only out_table is needed but save them here just in case *)
        val (in_table, in'_table, out_table, out'_table) = get_liveness_edges_rev(nodes, in_table, in'_table, out_table, out'_table, def_table, use_table, terminate_flag);

        val alltemps = get_all_temporaries(nodes, def_table, nil) (* Start it out as nil because it's a recursive call that builds the result *)

        (* Then get the fields that we need to put into the igraph *)
        val tnode = Temp.Table.empty;
        val gtemp = Graph.Table.empty;
        val (tnode, gtemp) = create_igraph_mappings(alltemps, tnode, gtemp, igraph);
        val moves = create_moves_list(alltemps, tnode, def, use, ismove, nodes);

        (* Make all of the edges based on the results, return value is not needed *)
        val garbage = make_graph_edges(igraph, nodes, def_table, out_table, tnode);

        (* TODO: Check this for correctness *)
        fun l_mapping(x:Graph.node) = case Graph.Table.look(out_table,x)
          of SOME(lst) => lst
           | NONE => (print "Node does not exist"; nil)


        (* Add in if debugginging *)
        val a = print("Results:\n");
        val a = print("In table:\n");
        val x = MakeGraph.dump_temp_table(in_table, nodes);
        val a = print("\nOut table:\n");
        val y = MakeGraph.dump_temp_table(out_table, nodes);
        val a = print("\nDef table:\n");
        val x = MakeGraph.dump_temp_table(def_table, nodes);
        val a = print("\nUse table:\n");
        val y = MakeGraph.dump_temp_table(use_table, nodes);
        val x = print("\n");
        val x = show_temps(alltemps);
        val x = print("\nFlow Graph Edges:\n");
        val x = show_edges(control, nodes); 
        val x = print("\nInterference Graph Edges:\n");
        val x = show_edges(igraph, Graph.nodes(igraph));
        val x = print("\nAdjacencies:\n");
        val x = show_adj(Graph.nodes(igraph));

      in
        (IGRAPH({graph=igraph, tnode=tnode, gtemp=gtemp, moves=moves}), l_mapping)
      end;

  fun test() = 
    let
      (* just use variables a,b,c represented as temporaries 1,2,3, and use the instructinos from page 212 in the book *)
      val i1 = Assem.OPER ({assem="assign", dst=[1], src=[], jump=NONE});                                                     (* a := 0 *)
      val i2 = Assem.LABEL({assem="filler", lab = Symbol.symbol("INST2")});                                                   (* Just a label so we can jump here *)
      val i3 = Assem.OPER ({assem="assign", dst=[2], src=[1], jump=NONE});                                                    (* b := a + 1 *)
      val i4 = Assem.OPER ({assem="assign", dst=[3], src=[2,3], jump=NONE});                                                  (* c := c + b *)
      val i5 = Assem.OPER ({assem="assign", dst=[1], src=[2], jump=NONE});                                                    (* a := b*2 *)
      val i6 = Assem.OPER ({assem="bool", dst=[], src=[1], jump=SOME([Symbol.symbol("INST2"), Symbol.symbol("INST7")])});     (* a<N *)
      val i7 = Assem.LABEL({assem="filler", lab = Symbol.symbol("INST7")});                                                   (* Just a label so we can jump here *)
      val i8 = Assem.OPER ({assem="return", dst=[], src=[3], jump=NONE});                                                     (* return c *)

      val (graph as Flow.FGRAPH({control, def, use, ismove}), nodes) = MakeGraph.instrs2graph(i1::i2::i3::i4::i5::i6::i7::i8::nil);
      val result = interferenceGraph(graph);
      
    in
      ()
    end

  (* after constructing the livenessMap, it is quite easy to
     construct the interference graph, just scan each node in
     the Flow Graph, add interference edges properly ... 
   *)


  (* Now that it knows which temporaries are live out at each node in the table, and it already knew which
     instructions are move instructions based on instrs2graph(), fill in the fields *)
  (* TODO: specify which kind of node it is! *)
  (* WARNING: This is going to be scrapped! Was old code! *)
  fun fill_in_graph_fields(node::tail, out_table, tnode, gtemp, moves) = 
        let
          val instr = getOpt(Graph.Table.look(out_table, node), []); (* This should never be NONE, it should get a list of temporaries *)
          (* TODO: This is not filled in! *)
          (* val new_tnode = Temp.Table.enter(tnode, node, ) *)
        in
          fill_in_graph_fields(tail, out_table, tnode, gtemp, moves)
        end
    | fill_in_graph_fields(nil, out_table, tnode, gtemp, moves) = (tnode, gtemp, moves)





end (* structure Liveness *)

     

                 