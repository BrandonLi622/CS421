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

    fun current_adjacent_nodes(node, gtemp, simplify_worklist) =
      let
        val adj_nodes = Graph.adj(node);
        val adj_nodes = map (fn(adj_node) => valOf(Graph.Table.look(gtemp, adj_node))) adj_nodes; (* Working with integers is easier *)

        val curr_adj = Liveness.set_difference(adj_nodes, simplify_worklist); (* Anything that was put into this worklist is not in current graph *)
      in
        curr_adj
      end

    fun is_member (element, lst) = List.exists(fn(lst_element) => element = lst_element) lst;

    fun are_currently_adj(node1, node2, gtemp, simplify_worklist) =
      let
        val curr_adj = current_adjacent_nodes(node1, gtemp, simplify_worklist);
        val node2_temp = valOf(Graph.Table.look(gtemp, node2));
      in
        is_member(node2_temp, curr_adj)
      end
    
    fun current_degree(node, gtemp, simplify_worklist) =
      let
        val curr_adj = current_adjacent_nodes(node, gtemp, simplify_worklist)
      in
        List.length(curr_adj)
      end

    (* Returns a tuple (node_num, is_guess) such that we have (node_num, false) if there is a node such that degree < k
       or returns (node_num, true) if it's doing the optimistic heuristic and just pushing onto the stack *)
    fun select_simplify_node(current_degrees, nodes, gtemp, reg_count, simplify_worklist) =
      let
        fun select_simplify_node_helper(current_degree::tail, nodes, gtemp, reg_count, simplify_worklist, node_num) =
              let
                val node = List.nth(nodes,node_num);
                val temp = valOf(Graph.Table.look(gtemp, node));
              in
                if (current_degree < reg_count andalso not(is_member(temp, simplify_worklist)))
                then (temp, false) 
                else select_simplify_node_helper(tail, nodes, gtemp, reg_count, simplify_worklist, node_num)
              end
              
          | select_simplify_node_helper(nil, nodes, gtemp, reg_count, simplify_worklist, node_num) = 
              let
                val all_temps = map (fn(node) => valOf(Graph.Table.look(gtemp, node))) nodes;
                val nodes_left = Liveness.set_difference(all_temps, simplify_worklist);
                val node = List.nth(nodes_left, 0); (* Just pick  *)
                val temp = valOf(Graph.Table.look(gtemp, node));
              in
                (temp, true)
              end
              
      in
        select_simplify_node_helper(current_degrees, nodes, gtemp, reg_count, simplify_worklist, 0)
      end

    fun simplify(nodes, gtemp, simplify_worklist, reg_count) =
      if List.length(simplify_worklist) = List.length(nodes)
      then simplify_worklist
      else
        let
          val current_degrees = map (fn(node) => current_degree(node, gtemp, simplify_worklist)) nodes
          val (selected_index, is_guess) = select_simplify_node(current_degrees, nodes, gtemp, reg_count, simplify_worklist);
          val node = List.nth(nodes, selected_index); (* TODO: What if there are 0 nodes? *)
          val temp = valOf(Graph.Table.look(gtemp, node));
        in
          simplify(nodes, gtemp, temp::simplify_worklist, reg_count)
        end

    

    fun color {interference as Liveness.IGRAPH({graph, tnode, gtemp, moves}), initial, registers} =  (*initial*)  
      let
        val alloc = Temp.Table.empty;
        val alloc = Temp.Table.enter(alloc, 1, "reg1"); (* temp a (number 1) goes into register 1 *)
        val alloc = Temp.Table.enter(alloc, 2, "reg1"); (* temp b (number 2) goes into register 1 *)
        val alloc = Temp.Table.enter(alloc, 3, "reg2"); (* temp c (number 3) goes into register 2 *)

        val simplify_worklist = []; (* Want to use it like a stack *)

        val nodes = Graph.nodes(graph);


      in
        alloc
      end;

end (* functor RegAllocGen *)
