(* Time to check if executing my running thread induces an operation on some other thread on some other node! *)
fun canInduce       {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]}
				    {node_name=this_node, port_name=this_port, opn=opn} = false
|   canInduce       {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, 
                    op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=[]}]}
					{node_name=this_node, port_name=this_port, opn=opn} = false
|   canInduce       {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,    
                    op_calls=({port_type=port_type, port_name=port_name1, unblk_list=unblk_list,  induced=induced,  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}
                    {node_name=this_node, port_name=this_port, opn=opn} = 

         (* In case of RMI or AMI *)
         if((comp_node = this_node) andalso (port_name1=this_port) andalso (port_type = "RMI_Receptacle" orelse port_type = "AMI_Receptacle" orelse port_type = "Facet") andalso (induced = false))	then
         			true 
         else
         	(* In case of Push Subscription! *)
         	if ((port_type = "Publisher") andalso ((comp_node = this_node) orelse (this_node = "ALL")) andalso (port_name1=this_port) andalso (call_exec_t = call_dur) andalso (induced = false)) then true
         	
         	else false;	

(* Main Induce function - Do the check and return an operation that needs to be enqueued *)		
fun induce_th {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, 
					 op_calls=({port_type=port_type, port_name=port_name1, unblk_list=unblk_list,  induced=induced,  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}
              {node_name=this_node, port_name=this_port, opn=opn} = 

            (* Check induced after executing! - on executing, if call_exec_t becomes equal to call_dur or q_t, then simultaneously induce operation on another component! *)
            if (canInduce 
            			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, 
            							op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name1, unblk_list=unblk_list,  induced=induced,  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

                    {node_name = this_node, port_name=this_port, opn=opn} = true) then 

                    	1`(opn)

            else empty;	


(* Handle Induce! Set induced to true to let the simulator know that induciton just happened *)
fun handle_induce {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, 
					 op_calls=({port_type=port_type, port_name=port_name1, unblk_list=unblk_list,  induced=induced,  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} =

				 {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, 
					 op_calls=({port_type=port_type, port_name=port_name1, unblk_list=unblk_list,  induced=true,  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}