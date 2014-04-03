(* liveness.sml *)

signature LIVENESS =
sig

  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table,
                 gtemp : Temp.temp Graph.Table.table,
                 moves : (Graph.node * Graph.node) list}

  
     val interferenceGraph : 
           Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list) 
   
   (*  val show : outstream * igraph -> unit
   *)

end (* signature LIVENESS *)

structure Liveness : LIVENESS = 
struct

  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table,
                 gtemp : Temp.temp Graph.Table.table,
                 moves : (Graph.node * Graph.node) list}

  (* To construct the interference graph, it is convenient to
     construct a liveness map at each node in the FlowGraph first.
     For each node in the flowgraph, i.e., for each assembly 
     instruction, we want to easily look up the set S of live 
     temporaries. 
   *)

  type liveSet = unit Temp.Table.table * Temp.temp list
  type livenessMap = liveSet Flow.Graph.Table.table

  fun interferenceGraph(Flow.FGRAPH({control=control,
                                     def=def,
                                     use=use,
                                     ismove=ismove})) = let
                                                          val iGraph = Graph.newGraph();
                                                          val tNode = Temp.Table.empty;
                                                          val gTemp = Graph.Table.empty;
                                                          val moves = nil;

                                                          (* TODO: What is the correct way to do this? *)
                                                          (* Also, this should probably print an error where it's a NONE... *)

                                                          (* Instead of hard coding ALL of this part, I grab the entries inserted
                                                             from instrs2graph() *)
                                                          fun lMapping(x:Graph.node) = case Graph.Table.look(use,x)
                                                                                          of SOME(l) => l
                                                                                           | NONE => (print "Node does not exist"; nil);
                                                        in
                                                          (IGRAPH({graph=iGraph, tnode=tNode, gtemp=gTemp, moves=moves}), lMapping)
                                                        end

                                 



  (* after constructing the livenessMap, it is quite easy to
     construct the interference graph, just scan each node in
     the Flow Graph, add interference edges properly ... 
   *)




end (* structure Liveness *)

     

                 