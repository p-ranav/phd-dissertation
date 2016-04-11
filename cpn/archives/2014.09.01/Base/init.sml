(* Init Enqueue - used to establish order *)
fun init_enq thread {nid=nid, can_sched=can_sched, node_ths=node_ths} = 
    {nid=nid, can_sched=can_sched, node_ths=(enqueue_thread thread node_ths)};

(* Init check - used to check node *)
fun init_guard {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} 
               {nid=nid, can_sched=can_sched, node_ths=node_ths} = 
      if (comp_node = nid) then true else false;