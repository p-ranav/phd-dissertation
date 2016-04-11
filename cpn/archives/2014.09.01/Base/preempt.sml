(* Check thread exec_t - has thread run for a clock tick? - the min time assigned to it when it was scheduled *)
fun preempt_check clock_value {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} =
    (clock_value - comp_st = clock_tick);


(* Main Preempt Guard! *)
fun preempt_guard {clock_node=clock_node, clock_value=clock_value, 
				 	schedule=schedule}
				  {nid=nid, can_sched=can_sched, node_ths=node_ths}
				  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = 

			if ((clock_node = comp_node) andalso (nid = comp_node) andalso (preempt_check clock_value {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = true)) 
			then true else false;	                   											

(* House keeping - Remove op - if needed - on preempt *)
fun remove_op {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = 
                          {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]}
| remove_op {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, 
             op_calls=[]}]} =
                     {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]}
| remove_op {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, 
             op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
            if(call_exec_t = call_dur andalso other_calls = []) then 
              {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} 
           else 
           	   if (call_exec_t = call_dur) then 
           	   		{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=other_calls}]}
           	   else
           	        {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, 
                          op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};


(* Helper to enqueue thread (back into thread priority queue) on preempt *)
fun pnq_helper {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} [] = 
                 [{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}]
    | pnq_helper {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}
            ({comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op2}::queue) = 
       if (comp_prio > comp_prio2) then 
               {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} :: 
              ({comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op2}::queue)
       else {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op2}::
               (pnq_helper {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} queue);


(* Call this enqueue function when you need to return a thread back to the thread queue - WHAT IF I CANT FIND THE PARTITION - non exhaustive if I cant find partition *)
fun enqueue_thread {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} 
                	({part=part, ths=ths}::rest) = 
       if (comp_part = part) then
           ({part=part, ths = (pnq_helper {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} ths)}::rest)
       else
           ({part=part, ths=ths}::(enqueue_thread {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} rest));


(* Main Preempt Function *)
fun preempt_th thread {nid=nid, can_sched=can_sched, node_ths=node_ths} =
   {nid=nid, can_sched=true, node_ths=(enqueue_thread (remove_op thread) node_ths)};   