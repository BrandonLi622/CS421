(* regalloc.sml *)

signature REG_ALLOC =
sig
   structure R : REGISTER_STD
   
   type allocation = Register.register Temp.Table.table

   val color : {interference : Liveness.igraph,
                initial : allocation,
                registers : R.register list} -> allocation

  (* Just for testing *)
  val string_sort_list : string list -> string list 
  val to_sorted_string_set : string list -> string list
  val string_set_difference : string list * string list -> string list
  val string_set_union : string list * string list -> string list

  val test : unit -> unit


end (* signature REG_ALLOC *)

functor RegAllocGen(Register : REGISTER_STD) : REG_ALLOC =
struct
   structure R : REGISTER_STD = Register

   type allocation = R.register Temp.Table.table (* I'm assuming string registers in my code *)

   (* The color function talkes an initial allocation table (which assigns
      temporary variables such as FP or SP into certain fixed registers)
      plus an interference graph and a list of registers, and returns
      a new allocation table (mapping from temporaries to registers).

      Notice, you don't need to implement spilling and coalescing. 
      Just do the "simplify" and then do the "select".
    *)

  (********************************************************************************************************************************************)
  (****************************************************        Set Utilities        ***********************************************************)
  (********************************************************************************************************************************************)

    (* Two membership functions for ints and strings *)
    fun is_int_member (element : int, lst) = List.exists(fn(lst_element) => element = lst_element) lst;
    fun is_string_member (element : string, lst) = List.exists(fn(lst_element) => element = lst_element) lst;

    (* Right now is actually just insertion sort *)
    fun string_sort_list(lis : string list) = let
                                                fun insert(n, h::t) = if n < h then n::h::t else h::insert(n, t)
                                                  | insert(n, []) = [n]
                                              in
                                                foldl insert [] lis
                                              end;

    (* Does all of the hard work for to_sorted_set *)
    fun to_sorted_string_set_helper(a::b::xs : string list) = if a = b then to_sorted_string_set_helper(a::xs) else a::to_sorted_string_set_helper(b::xs)
      | to_sorted_string_set_helper(a::nil) = a::nil
      | to_sorted_string_set_helper(nil) = nil; 

    (* Removes all duplicates from and sorts @lis *)
    fun to_sorted_string_set(lis) = to_sorted_string_set_helper(string_sort_list(lis));

    fun string_set_union(set1, set2) = to_sorted_string_set(set1@set2);

    fun string_set_difference(set1 as a::xs : string list, set2 : string list) = if List.exists (fn b => if a = b then true else false) set2
                                                                                 then string_set_difference(xs, set2)
                                                                                 else a::string_set_difference(xs, set2)
      | string_set_difference(nil, _) = nil;




  (********************************************************************************************************************************************)
  (****************************************************       Graph Utilities       ***********************************************************)
  (********************************************************************************************************************************************)


    (* Gives the nodes that are currently adjacent to a particular node, which we need during the simplify and select phases because
       we will be working with subsets of the whole graph *)
    fun current_adjacent_nodes(node, gtemp, simplify_worklist) =
      let
        val adj_nodes = Graph.adj(node);
        val adj_nodes = map (fn(adj_node) => valOf(Graph.Table.look(gtemp, adj_node))) adj_nodes; (* Working with integers is easier *)

        val curr_adj = Liveness.set_difference(adj_nodes, simplify_worklist); (* Anything that was put into this worklist is not in current graph *)
      in
        curr_adj
      end

    fun are_currently_adj(node1, node2, gtemp, simplify_worklist) =
      let
        val curr_adj = current_adjacent_nodes(node1, gtemp, simplify_worklist);
        val node2_temp = valOf(Graph.Table.look(gtemp, node2));
      in
        is_int_member(node2_temp, curr_adj)
      end
    
    fun current_degree(node, gtemp, simplify_worklist) =
      let
        val curr_adj = current_adjacent_nodes(node, gtemp, simplify_worklist)
      in
        List.length(curr_adj)
      end



  (********************************************************************************************************************************************)
  (****************************************************  Coloring Algorithm Phases  ***********************************************************)
  (********************************************************************************************************************************************)

    (* Returns a tuple (node_num, is_guess) such that we have (node_num, false) if there is a node such that degree < k
       or returns (node_num, true) if it's doing the optimistic heuristic and just pushing onto the stack *)
    fun select_simplify_node(current_degrees, nodes, gtemp, reg_count, simplify_worklist) =
      let
        fun select_simplify_node_helper(current_degree::tail, nodes, gtemp, reg_count, simplify_worklist, node_num) =
              let
                val node = List.nth(nodes,node_num);
                val temp = valOf(Graph.Table.look(gtemp, node));
              in
                if (current_degree < reg_count andalso not(is_int_member(temp, simplify_worklist)))
                then (temp, false) 
                else select_simplify_node_helper(tail, nodes, gtemp, reg_count, simplify_worklist, node_num)
              end
              
          | select_simplify_node_helper(nil, nodes, gtemp, reg_count, simplify_worklist, node_num) = 
              let
                val all_temps = map (fn(node) => valOf(Graph.Table.look(gtemp, node))) nodes;
                val nodes_left = Liveness.set_difference(all_temps, simplify_worklist);
                val temp = List.nth(nodes_left, 0); (* Just pick  *)
              in
                (temp, true)
              end
              
      in
        select_simplify_node_helper(current_degrees, nodes, gtemp, reg_count, simplify_worklist, 0)
      end

    (* The simplify phase of the algorithm will keep on removing nodes from the graph and adding them to a worklist
        for each node that has degree less than reg_count. Hopefully, all of the nodes eventually get removed from
        the graph, but if we get stuck, then take optimistic guesses. *)
    fun simplify(nodes, gtemp, simplify_worklist, reg_count) =
      if List.length(simplify_worklist) = List.length(nodes)
      then simplify_worklist
      else
        let
          val current_degrees = map (fn(node) => current_degree(node, gtemp, simplify_worklist)) nodes
          (* is_guess is not actually used here but this program could be extended to do spilling and stuff, in which
             case it might be useful to have is_guess *)
          val (temp, is_guess) = select_simplify_node(current_degrees, nodes, gtemp, reg_count, simplify_worklist);
        in
          simplify(nodes, gtemp, temp::simplify_worklist, reg_count)
        end

    exception cannot_color;

    (* The select phase considers each node on the worklist one at a time and tries to assign it a color based on
       the colors of adjacent nodes. It throws cannot_color if we run out of colors when trying to color a
       particular node *)
    fun select(simplify_worklist as temp::tail, registers, allocation_table, gtemp, tnode) =
          let
            val node = valOf(Temp.Table.look(tnode, temp));
            val adj_temps = current_adjacent_nodes(node, gtemp, simplify_worklist);
            val adj_colors = to_sorted_string_set(foldl (fn(t, reg_set) => valOf(Temp.Table.look(allocation_table, t))::reg_set) [] adj_temps)
            val legal_colors = string_set_difference(registers, adj_colors);
            val color = if List.null(legal_colors) then raise cannot_color else hd(legal_colors);

            val allocation_table = Temp.Table.enter(allocation_table, temp, color)
          in
            select(tail, registers, allocation_table, gtemp, tnode)
          end
      | select(nil, registers, allocation_table, gtemp, tnode) = allocation_table;

  (********************************************************************************************************************************************)
  (****************************************************           color()           ***********************************************************)
  (********************************************************************************************************************************************)

    fun color {interference as Liveness.IGRAPH({graph, tnode, gtemp, moves}), initial, registers} =
      let
        val allocation_table = initial;
        val nodes = Graph.nodes(graph);
        val simplify_worklist = []; (* Want to use it like a stack *)
        val simplify_worklist = simplify(nodes, gtemp, simplify_worklist, List.length(registers))
      in
        select(simplify_worklist, registers, allocation_table, gtemp, tnode)
      end;


  (********************************************************************************************************************************************)
  (****************************************************     Debugging Functions     ***********************************************************)
  (********************************************************************************************************************************************)

    (* Just for testing *)
    fun dump_allocation_table(allocation_table, temps) = 
       (print("\nAssignments\n");
        map (fn temp => print("Temp: " ^ Int.toString(temp) ^ " => " ^ valOf(Temp.Table.look(allocation_table, temp)) ^ "\n")) temps; (* Will be ~1 for labels or bad instructions *)
        ())

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

        val (flowgraph as Flow.FGRAPH({control, def, use, ismove}), nodes) = MakeGraph.instrs2graph(i1::i2::i3::i4::i5::i6::i7::i8::nil);
        val (igraph as Liveness.IGRAPH({graph, tnode, gtemp, moves}), liveness_map) = Liveness.interferenceGraph(flowgraph);

        val all_nodes = Graph.nodes(graph);
        val all_temps = map (fn(node) => valOf(Graph.Table.look(gtemp, node))) all_nodes;

        val reg_list = ["reg1", "reg2", "reg3", "reg4", "reg5", "reg6", "reg7", "reg8", "reg9", "reg10", "reg11", "reg12", "reg13", "reg14", "reg15"]; (* Some sample register list *)
        val initial_allocation = Temp.Table.empty; (* Assume an empty starting table *)
        val color_input = {interference = igraph, initial = initial_allocation, registers = reg_list};

        val allocation = color(color_input);
      in
        dump_allocation_table(allocation, all_temps)
      end

end (* functor RegAllocGen *)
