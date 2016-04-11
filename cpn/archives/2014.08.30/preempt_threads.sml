(* Check thread exec_t - has thread run for a clock tick? - the min time assigned to it when it was scheduled *)
fun preempt_guard {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
				  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} =
    (clock_value - comp_st = clock_tick);


(* Main preempt guard *)
fun get_all_preempt_threads node_clock_list [] = []
  | get_all_preempt_threads 
                  node_clock_list
				  ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::other_threads) = 

		if (preempt_guard (hd (get_node_clock comp_node node_clock_list))  
						  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = true) then 
			[{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}]
			^^(get_all_preempt_threads node_clock_list other_threads)
		else 
			  (get_all_preempt_threads node_clock_list other_threads);

(* Handle Preempt_Thread *)
fun handle_preempt node_clock_list [] = []
  | handle_preempt node_clock_list
					[{comp_node=comp_node, comp_name=comp_name, 
						comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}] = 

			if (preempt_guard (hd (get_node_clock comp_node node_clock_list))
			                {comp_node=comp_node, comp_name=comp_name, 
						comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = true ) 
				then
					[]
			else 
				[{comp_node=comp_node, comp_name=comp_name, 
						comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}];  


(* Main Preempt thread function *)
fun preempt_threads nths_list
					running_threads
					node_clock_list = 
		(enqueue_thread_list (get_all_preempt_threads node_clock_list running_threads) nths_list);
