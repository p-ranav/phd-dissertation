(* Can THIS thread run? *)
fun can_run {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		    {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = false
  | can_run {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		    {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=[]}]} = false
  | can_run {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
  			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
  		if ((clock_value - comp_st < clock_tick) andalso 
  			(call_exec_t <= call_dur)) then true 
  		else false; 

(* Can the any one running thread "execute" - is there anything TO execute; is the call complete?; is the op complete? - check all*)
fun can_execute_ths node_clock_list [] = false
  | can_execute_ths node_clock_list 
				   ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::other_threads) = 
			(can_run (hd(get_node_clock comp_node node_clock_list)) 
					{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op})
			 orelse (can_execute_ths node_clock_list other_threads);

(* Main Execute Guard *)
fun execute_guard node_clock_list
				  node_ths_list
				  running_threads
				  timers
				  opn_list = 

		if ((timer_guard node_clock_list timers = false) andalso
			(enqueue_guard opn_list node_clock_list = false) andalso
			(can_execute_ths node_clock_list running_threads) = true) then true else false;

(*------------------------------------------------------------------------------------------------------*)

(* Remove operation if completed - Remove call if completed - Remove anything needed *)
fun handle_opn {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
		 		comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

		(* Case 0: RMI call - return [] because thread gets moved to blocked thread list *)
		if (port_type = "RMI_Receptacle" andalso call_dur = call_exec_t) then 
			[]
		else
			 (* Case 1: Whole Operation is completed *)
			 if (call_exec_t = call_dur andalso other_calls = []) then 
			 	[{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
			 		comp_st=comp_st, comp_op=[]}]
			 else 
			 	 (* Case 2: Call is complete but operation is not *)
			 	 if (call_exec_t = call_dur) then 
			 	 	[{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
			 		comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=other_calls}]}]
			 	 (* Case 3: Neither is complete - Leave thread as is *)
			 	 else 
			 	 	[{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
			 		comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}];

(* No timer expiry WITHIN THE NEXT CLOCK TICK *)
fun no_timer_expiry_this_node clock_value node_name [] = true 
  | no_timer_expiry_this_node clock_value node_name ({t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}::other_timers) = 
	if ((t_nid = node_name) andalso (t_off < clock_value + clock_tick)) then false
	else true andalso (no_timer_expiry_this_node clock_value node_name other_timers);

fun next_timer_offset [{t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}] = t_off
  |  next_timer_offset ({t_nid=t_nid1, t_pr=t_pr1, t_off=t_off1, t_op=t_op1}::{t_nid=t_nid2, t_pr=t_pr2, t_off=t_off2, t_op=t_op2}::rest) = 
           if (t_off1 <= t_off2) then 
                   next_timer_offset ({t_nid=t_nid1, t_pr=t_pr1, t_off=t_off1, t_op=t_op1}::rest)
           else 
                   (next_timer_offset ({t_nid=t_nid2, t_pr=t_pr2, t_off=t_off2, t_op=t_op2}::rest));

(* Find the next immediate timer offset *)
fun when_is_next_timer clock_value node_name timers = 
		((next_timer_offset (get_timers node_name timers)) - clock_value);

(* Return Minimum of two values *)
fun find_min value1 value2 = 
      if (value1 < value2) then value1
      else 
         if (value2 > value1) then value2
         else value1;

(* Handle Local call - Piece of code *)
(* Decide by how much you can progress time *)
(* (1) To next timer, (2) To completion of the call, (3) To next preempt point *)
fun handle_code {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		 		timers 
				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

		(* Call just started - Call will NOT end within 1 clock tick - no timer expiry on this node within the next clock tick - then just push time forward *)
		if ((call_exec_t + clock_tick <= call_dur) andalso (no_timer_expiry_this_node clock_value comp_node timers =  true)) then
			if (call_exec_t + clock_tick < call_dur) then 
				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t+(clock_tick-(clock_value-comp_st)), call_dur=call_dur}::other_calls)}]} 
			else 
				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t+clock_tick, call_dur=call_dur}::other_calls)}]} 
		(* If one of the above conditions is false... *)
		else 
			(* Call will end before the next clock tick and there are no timers to worry about *)
			if ((call_exec_t + clock_tick > call_dur) andalso (no_timer_expiry_this_node clock_value comp_node timers =  true)) then
				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t+(call_dur-call_exec_t), call_dur=call_dur}::other_calls)}]}
			else
				(* Call will end before the next clock tick and there ARE timers to worry about *)
				if ((call_exec_t + clock_tick > call_dur) andalso (no_timer_expiry_this_node clock_value comp_node timers =  false)) then 
					{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t+(find_min (call_dur-call_exec_t) (when_is_next_timer clock_value comp_node timers)), call_dur=call_dur}::other_calls)}]}
				(* Call WILL NOT end before next clock tick AND THERE ARE timers to worry about - just jump to next timer offset so the timers can fire *)
				else
					{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t+(when_is_next_timer clock_value comp_node timers), call_dur=call_dur}::other_calls)}]};

(* Handle Interaction - Since q_t and pr_t are removed, the interaction does not take up time - Only purpose is to induce operations - So return the thread as is *)
fun handle_interaction {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn,
		 													 	op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
		 			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn,
		 													 	op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};


(* Execute one thread - Make the choice - How to progress the time for this thread execution *)
fun exec {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		 timers 
		 {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

	case port_type of "Local" => (handle_code {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		 									 timers 
		 									 {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]})
		 			| "RMI_Receptacle" => (handle_interaction  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn,
		 													 	op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]})
		 			| "AMI_Receptacle" => (handle_interaction  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn,
		 													 	op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]})
		 			| "Facet" => (handle_interaction  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn,
		 													 	op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]})
		 			| "Publisher" => (handle_interaction  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn,
		 													 	op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]})
		 			| "Subscriber" => (handle_interaction  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn,
		 													 	op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]});

(* Main execute thread function - Run exec on all running threads *)
fun execute_thread node_clock_list timers [] = []
  | execute_thread node_clock_list
  				   timers
				   ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::other_threads) = 
		(handle_preempt node_clock_list (handle_opn (exec (hd (get_node_clock comp_node node_clock_list))
			  timers
			  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op})))^^
		(execute_thread node_clock_list timers other_threads);


