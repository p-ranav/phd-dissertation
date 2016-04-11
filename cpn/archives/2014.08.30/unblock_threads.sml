(* Accumulate a list of threads that CAN be unblocked right now *)
fun find_ths_to_unblock [] 
					    {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						  comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op}
                        = []

  | find_ths_to_unblock ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
							op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="RMI_Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}::other_blked_threads)

						 {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						    comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[]} = 

				         []

  | find_ths_to_unblock ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
							op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="RMI_Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}::other_blked_threads)

						{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 							op_st=op_st2, op_dv=op_dv2, op_calls=[]}]} = 

				         []

  | find_ths_to_unblock ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
							op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="RMI_Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}::other_blked_threads)

							{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 								op_st=op_st2, op_dv=op_dv2, op_calls=({port_type=port_type2, port_name=port_name2, unblk_list=unblk_list2, induced=induced2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]} = 

 					if ((other_calls2 = []) andalso (contains unblk_list2 [{node_name=comp_node,
 							 	comp_name=comp_name, port_name=port_name}]) = true 
 							 	andalso (call_exec_t2  = call_dur2)) then 

			 			[{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=other_calls}]}]
			 			
						^^(find_ths_to_unblock other_blked_threads 
									{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
			 								op_st=op_st2, op_dv=op_dv2, op_calls=({port_type=port_type2, port_name=port_name2, unblk_list=unblk_list2, induced=induced2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]})

			 		else 
			 			(find_ths_to_unblock other_blked_threads 
									{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
			 								op_st=op_st2, op_dv=op_dv2, op_calls=({port_type=port_type2, port_name=port_name2, unblk_list=unblk_list2, induced=induced2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]});



(* Iterate through all running threads and chcek and fine threads to unblock *)
fun find_all_unblked_threads all_blocked_threads
							 [] = []
  | find_all_unblked_threads all_blocked_threads
							 ({comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						  		comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op}::other_threads) = 
				(find_ths_to_unblock all_blocked_threads 
								{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						  		comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op})
				^^(find_all_unblked_threads all_blocked_threads other_threads);


(* Enqueue a list of threads back into the node_ths_list - Basically call establish_order for all threads *)
fun enqueue_thread_list []
						nths_list = nths_list
  | enqueue_thread_list (thread::thread_list)
						 nths_list = 

			(enqueue_thread_list thread_list 
							(establish_order thread nths_list));

(* Main Unblock thread function *)
fun unblock_threads nths_list
					running_threads
					all_blocked_threads = 
		(enqueue_thread_list (find_all_unblked_threads all_blocked_threads running_threads) nths_list);


(* Remove threads from list *)
fun remove_these_threads blocked_threads
						 [] = blocked_threads
  | remove_these_threads blocked_threads
						 (thread::other_unblocked_threads) = 
		(remove_these_threads (rmall thread blocked_threads) other_unblocked_threads);

(* Update Blocked Threads - MAIN *)
fun update_blocked_threads running_threads
						   all_blocked_threads = 
			(remove_these_threads all_blocked_threads
							(find_all_unblked_threads all_blocked_threads running_threads));

						 