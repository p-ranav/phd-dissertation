(* Main thread blocking function *)
fun block_threads [] = []
  | block_threads 
         ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
		 	comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}::other_threads) = 

		 if (port_type = "RMI_Receptacle" andalso call_exec_t = call_dur) then 
		 	          [{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
		 				comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}]
		 	^^(block_threads other_threads)
		 else 
		 	(block_threads other_threads);

