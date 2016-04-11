(* Find Induce Operations *)
fun find_induce_opns {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st,
  						 op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name1, unblk_list=unblk_list,  induced=induced,  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}
  					  [] = []
  | find_induce_opns {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
				 		comp_st=comp_st, comp_op=[]}
					 ({node_name=this_node, port_name=this_port, opn=opn}::other_opns) = []
  | find_induce_opns {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, 
  						comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=[]}]}
					 ({node_name=this_node, port_name=this_port, opn=opn}::other_opns) = []
  | find_induce_opns {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st,
  						 op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name1, unblk_list=unblk_list,  induced=induced,  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}
  					 ({node_name=this_node, port_name=this_port, opn=opn}::other_opns) = 

  			if (comp_node = this_node andalso port_name1 = this_port andalso (port_type = "RMI_Receptacle" orelse port_type = "AMI_Receptacle" orelse port_type = "Facet") andalso (induced = false)) then 
  					(opn)::(find_induce_opns {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio,
  											 	op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name1, unblk_list=unblk_list,  induced=induced,  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} 
  										other_opns)
  			else 
  				if ((port_type = "Publisher") andalso ((comp_node = this_node) orelse (this_node = "ALL")) 
  						andalso (port_name1=this_port) andalso (call_exec_t = call_dur) andalso (induced = false)) then
  					(opn)::(find_induce_opns {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio,
  											 	op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name1, unblk_list=unblk_list,  induced=induced,  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} 
  										other_opns)
  				else
  					(find_induce_opns {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio,
  											 	op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name1, unblk_list=unblk_list,  induced=induced,  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} 
  										other_opns)



(* Main Induction Guard *)
fun induce_opns [] ({node_name=this_node, port_name=this_port, opn=opn}::other_opns) = []
  | induce_opns ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
				 comp_st=comp_st, comp_op=comp_op}::other_threads)

				 ({node_name=this_node, port_name=this_port, opn=opn}::other_opns) = 

		(find_induce_opns {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
						   comp_st=comp_st, comp_op=comp_op}
						  ({node_name=this_node, port_name=this_port, opn=opn}::other_opns))^^
				(induce_opns other_threads ({node_name=this_node, port_name=this_port, opn=opn}::other_opns));