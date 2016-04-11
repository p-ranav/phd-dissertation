(* Pick random thread from the list *)
fun pick (first::rest) = 
  let
  val i = discrete (0, (List.length (first::rest)) -1)
  in
  SOME (List.nth ((first::rest), i), List.take ((first::rest), i) ^^ List.drop ((first::rest), i+1))
  end
  | pick [] = NONE;

(* Helper to enqueue thread (back into thread priority queue) *)
fun pnq_helper {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} [] = 
                 [{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}]
    | pnq_helper {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}
            ({comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op2}::queue) = 
       if (comp_prio > comp_prio2) then 
               {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} :: 
              ({comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op2}::queue)
       else {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op2}::
               (pnq_helper {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} queue);

(* Call this enqueue function when you need to return a thread back to the thread queue - non exhaustive if I cant find partition *)
fun enqueue_thread {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} 
                	({part=part, ths=ths}::rest) = 
       if (comp_part = part) then
           ({part=part, ths = (pnq_helper {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} ths)}::rest)
       else
           ({part=part, ths=ths}::(enqueue_thread {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} rest));


(* Main thread enqueue function *)
fun establish_order 
        {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, 
                  comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} 
        ({nid=nid, can_sched=can_sched, node_ths=node_ths}::other_nths) = 

    if (comp_node = nid) then 
    	({nid=nid, can_sched=can_sched, node_ths=(enqueue_thread {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} node_ths)}::other_nths)
   	else 
   		({nid=nid, can_sched=can_sched, node_ths=node_ths}::(establish_order {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, 
                  comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} other_nths));
