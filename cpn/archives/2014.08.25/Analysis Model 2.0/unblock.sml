(* Is it time to unblock an RMI-blocked thread? Lets see - we need a call on a facet to complete; and we need to unblock the right thread - i.e. the thread to be unblocked should be in its unblk_list! *)
fun canUnblock {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[]}

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="RMI_Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

  			{nid=nid, can_sched=can_sched, node_ths=node_ths} = false

 | canUnblock {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 				op_st=op_st2, op_dv=op_dv2, op_calls=[]}]}

				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} 

	  			{nid=nid, can_sched=can_sched, node_ths=node_ths} = false

 | canUnblock {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 				op_st=op_st2, op_dv=op_dv2, op_calls=({port_type=port_type2, port_name=port_name2, unblk_list=unblk_list2, induced=induced2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]}

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} 

  			{nid=nid, can_sched=can_sched, node_ths=node_ths} =

        if ((other_calls2 = []) andalso (contains unblk_list2 [{node_name=comp_node, comp_name=comp_name, port_name=port_name}]) = true andalso (call_exec_t2  = call_dur2) andalso (nid = comp_node) ) then true else false;

(* Update Exec Time after unblocking! *)
fun update_exec_t {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 				op_st=op_st2, op_dv=op_dv2, op_calls=({port_type=port_type, port_name=port_name2, unblk_list=unblk_list2, induced=induced2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]}

					{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="RMI_Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}  = 


  				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="RMI_Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + call_exec_t2, call_dur=call_dur + call_exec_t2}::other_calls)}]};

(* Main unblock thread function *)
fun unblock_th thread blocked_thread {nid=nid, can_sched=can_sched, node_ths=node_ths} =
   {nid=nid, can_sched=can_sched, node_ths=(enqueue_thread (update_exec_t thread blocked_thread) node_ths)}; 
