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

		if ((reached_limit node_clock_list = false) andalso
			(timer_guard node_clock_list timers = false) andalso
			(enqueue_guard opn_list node_clock_list = false) andalso
			(can_execute_ths node_clock_list running_threads) = true) then true else false;

(*------------------------------------------------------------------------------------------------------*)

(* Mindless partition switch - No checks *)
fun switch_partition [] = []
  | switch_partition [{part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}] =
  					 [{part_name=part_name, exec_t=0, dur=dur, pr=pr, off=off+pr}]
  | switch_partition ({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::rest) = 
					  (rest^^[{part_name=part_name, exec_t=0, dur=dur, pr=pr, off=off+pr}]);

(* Progress Clock by x amount of time *)
fun progress_clock x {clock_node=clock_node, clock_value=clock_value, schedule=schedule} = 
					  {clock_node=clock_node, clock_value=(clock_value+x), schedule=schedule};

(* Progress Schedule by x amount of time *)
fun progress_schedule x {clock_node=clock_node, clock_value=clock_value, schedule=[]} = 
			{clock_node=clock_node, clock_value=clock_value, schedule=[]}
  | progress_schedule x {clock_node=clock_node, clock_value=clock_value, 
						  schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::rest)} = 
		if (exec_t + x < dur) then
			{clock_node=clock_node, clock_value=clock_value, 
						schedule=({part_name=part_name, exec_t = exec_t + x, dur=dur, pr=pr, off=off}::rest)}
		else 
			(progress_schedule (x - (dur - exec_t))
				{clock_node=clock_node, clock_value=clock_value, 
								   schedule=(switch_partition ({part_name=part_name, exec_t = exec_t + x, dur=dur, pr=pr, off=off}::rest))});

(* Update Node Clock - Progress clock value and partition exec_t time by x *)
fun update_node_clock x {clock_node=clock_node, clock_value=clock_value, schedule=schedule} = 
				 (progress_schedule x 
				 (progress_clock x 
				       {clock_node=clock_node, clock_value=clock_value, schedule=schedule}));

(* Progress node clock by this_much on this_node *)
fun progress_clock [] node_name progress_time = [] 
  | progress_clock ({clock_node=clock_node, clock_value=clock_value, schedule=schedule}::other_node_clocks) node_name progress_time = 
  		if (node_name = clock_node) then 
  			(update_node_clock progress_time {clock_node=clock_node, clock_value=clock_value, schedule=schedule})::other_node_clocks
  		else 
  			{clock_node=clock_node, clock_value=clock_value, schedule=schedule}::(progress_clock other_node_clocks node_name progress_time);	

(* Given a running thraed, find out the call_exec_t of the currently executing call *)
fun get_call_exec_t {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
		 				comp_st=comp_st, comp_op=[]} = 0
  | get_call_exec_t {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
		 				comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=[]}]}
		 				= 0
  | get_call_exec_t {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
		 				comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
		 			call_exec_t;

(* Check thread exec_t - has thread run for a clock tick? - the min time assigned to it when it was scheduled *)
fun preempt_guard {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
				  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} =
    (clock_value - comp_st = clock_tick);



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

fun update_clocks node_clock_list timers [] = node_clock_list
  | update_clocks   node_clock_list
					timers
					({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
		 				comp_st=comp_st, comp_op=comp_op}::other_threads) = 

		 (update_clocks 
		 	(progress_clock node_clock_list comp_node
		 		( (get_call_exec_t   (exec (hd (get_node_clock comp_node node_clock_list)) 
		 									 timers   
		 									{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
		 								      comp_st=comp_st, comp_op=comp_op}
		 						     )
		 		   ) 
		 		- (get_call_exec_t {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
		 								comp_st=comp_st, comp_op=comp_op}))
		 	)

		 	timers
		 	other_threads);


(* Main execute thread function - Run exec on all running threads *)
fun execute_thread node_clock_list timers [] = []
  | execute_thread node_clock_list
  				   timers
				   ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::other_threads) = 
		(handle_preempt 
			  (update_clocks node_clock_list timers
			  		({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::other_threads)) 
			  (handle_opn (exec (hd (get_node_clock comp_node node_clock_list))
			  		      timers
			  			  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op})))^^
		(execute_thread node_clock_list timers other_threads);

(* Enqueue a list of threads back into the node_ths_list - Basically call establish_order for all threads *)
fun enqueue_thread_list []
						nths_list = nths_list
  | enqueue_thread_list (thread::thread_list)
						 nths_list = 

			(enqueue_thread_list thread_list 
							(establish_order thread nths_list));

(* Main preempt guard *)
fun get_all_preempt_threads node_clock_list original_nc timers [] = []
  | get_all_preempt_threads 
                  node_clock_list
                  original_nc
                  timers
				  ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::other_threads) = 

		if (preempt_guard (hd (get_node_clock comp_node node_clock_list))  
						  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = true) then 
			(handle_opn (exec (hd (get_node_clock comp_node original_nc)) 
							timers
					{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}))
			^^(get_all_preempt_threads node_clock_list original_nc timers other_threads)
		else 
			  (get_all_preempt_threads node_clock_list original_nc timers other_threads);

(* Main Preempt thread function *)
fun preempt_threads nths_list
					running_threads
					node_clock_list
					timers = 
		(enqueue_thread_list (get_all_preempt_threads 
								(update_clocks node_clock_list timers running_threads) 
								node_clock_list
								timers
								running_threads) nths_list);




(* Main guard to jump time to either the next timer or next partition *)
(* This is the lowest priority transition - so literally every other guard should fail *)
fun jump_time_guard node_clock_list
					node_ths_list
					node_cmq_list
					running_ths
					timers
					waiting_opns = 

		if (  (reached_limit node_clock_list = false) andalso
			  (timer_guard node_clock_list timers = false) andalso
			  (enqueue_guard waiting_opns node_clock_list = false) andalso
			  (schedule_guard node_clock_list node_cmq_list node_ths_list timers waiting_opns running_ths = false) andalso
			  (execute_guard node_clock_list node_ths_list running_ths timers waiting_opns = false )) then true else false;


(* No timer expiry before the next partition *)
fun no_timer_expiry_this_partition 
                      {clock_node=clock_node, clock_value=clock_value, schedule=[]}
								   timers = false
  | no_timer_expiry_this_partition 
              {clock_node=clock_node, clock_value=clock_value, 
              schedule=({part_name=part_name, exec_t = exec_t, dur=dur, pr=pr, off=off}::rest)} 
  							   [] = true 

  | no_timer_expiry_this_partition 
              {clock_node=clock_node, clock_value=clock_value, 
              schedule=({part_name=part_name, exec_t = exec_t, dur=dur, pr=pr, off=off}::rest)} 
  				({t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}::other_timers) = 

  		if ((t_nid = clock_node) andalso (t_off < (clock_value + (dur-exec_t)))) then 
  				false
  		else 
  			(true andalso (no_timer_expiry_this_partition
  				{clock_node=clock_node, clock_value=clock_value, 
              schedule=({part_name=part_name, exec_t = exec_t, dur=dur, pr=pr, off=off}::rest)}
                 other_timers));

(* Jump time on each node and return updated node_clock *)
fun jump_time_this_node timers 
				{clock_node=clock_node, clock_value=clock_value, schedule=[]} = 
			(update_node_clock (when_is_next_timer clock_value clock_node timers)
					{clock_node=clock_node, clock_value=clock_value, schedule=[]})
  | jump_time_this_node timers 
					{clock_node=clock_node, clock_value=clock_value, 
					 schedule=({part_name=part_name, exec_t = exec_t, dur=dur, pr=pr, off=off}::rest)} = 
		if (no_timer_expiry_this_partition
				{clock_node=clock_node, clock_value=clock_value, 
				   schedule=({part_name=part_name, exec_t = exec_t, dur=dur, pr=pr, off=off}::rest)}
				timers = true) then 
			(update_node_clock (dur-exec_t) 
					{clock_node=clock_node, clock_value=clock_value, 
					    schedule=({part_name=part_name, exec_t = exec_t, dur=dur, pr=pr, off=off}::rest)})
		(* Find and jump to next timer *)
		else 
		    (update_node_clock (when_is_next_timer clock_value clock_node timers)
		    		{clock_node=clock_node, clock_value=clock_value, 
		    		schedule=({part_name=part_name, exec_t = exec_t, dur=dur, pr=pr, off=off}::rest)});

(* Main jump time function *)
(* HUGE Assumption here - Assuming no inter-node interactions *)
(* Therefore, treating each node as independent of the rest *)
(* So, decision behind jumping the clock on one node does not depend on state of any other node *)
fun jump_time timers [] = []
  | jump_time timers
			  ({clock_node=clock_node, clock_value=clock_value, schedule=schedule}::other_node_clocks) =

		(jump_time_this_node timers {clock_node=clock_node, clock_value=clock_value, schedule=schedule})
		::(jump_time timers other_node_clocks);


(* Accumulate a list of threads that CAN be unblocked right now *)
fun find_ths_to_unblock [] 
					    {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						  comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op2}
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


(* Accumulate a list of threads that CAN be removed right now from blocked threads list *)
fun find_ths_to_remove [] 
					    {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						  comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op2}
                        = []

  | find_ths_to_remove ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
							op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="RMI_Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}::other_blked_threads)

						 {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						    comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[]} = 

				         []

  | find_ths_to_remove ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
							op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="RMI_Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}::other_blked_threads)

						{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 							op_st=op_st2, op_dv=op_dv2, op_calls=[]}]} = 

				         []

  | find_ths_to_remove ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
							op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="RMI_Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}::other_blked_threads)

							{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 								op_st=op_st2, op_dv=op_dv2, op_calls=({port_type=port_type2, port_name=port_name2, unblk_list=unblk_list2, induced=induced2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]} = 

 					if ((other_calls2 = []) andalso (contains unblk_list2 [{node_name=comp_node,
 							 	comp_name=comp_name, port_name=port_name}]) = true 
 							 	andalso (call_exec_t2  = call_dur2)) then 

			 			[{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
							op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type="RMI_Receptacle", port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}]
			 			
						^^(find_ths_to_remove other_blked_threads 
									{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
			 								op_st=op_st2, op_dv=op_dv2, op_calls=({port_type=port_type2, port_name=port_name2, unblk_list=unblk_list2, induced=induced2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]})

			 		else 
			 			(find_ths_to_remove other_blked_threads 
									{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
			 								op_st=op_st2, op_dv=op_dv2, op_calls=({port_type=port_type2, port_name=port_name2, unblk_list=unblk_list2, induced=induced2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]});


(* Find all threads to be removed *)
fun find_all_removed_threads all_blocked_threads node_clock_list timers
							 [] = []
  | find_all_removed_threads all_blocked_threads node_clock_list timers
							 ({comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						  		comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op}::other_threads) = 
				(find_ths_to_remove all_blocked_threads 
								(exec   (hd (get_node_clock comp_node2 node_clock_list))  timers  
									{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						  		            comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op}) )
				^^(find_all_removed_threads all_blocked_threads node_clock_list timers other_threads);



(* Iterate through all running threads and chcek and fine threads to unblock *)
fun find_all_unblked_threads all_blocked_threads node_clock_list timers
							 [] = []
  | find_all_unblked_threads all_blocked_threads node_clock_list timers
							 ({comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						  		comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op}::other_threads) = 
				(find_ths_to_unblock all_blocked_threads 
								(exec   (hd (get_node_clock comp_node2 node_clock_list))  timers  
									{comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, 
						  		            comp_prio=comp_prio2, comp_st=comp_st2, comp_op=comp_op}) )
				^^(find_all_unblked_threads all_blocked_threads node_clock_list timers other_threads);

(* Main Unblock thread function *)
fun unblock_threads nths_list
					node_clock_list
					timers
					running_threads
					all_blocked_threads = 
		(enqueue_thread_list (find_all_unblked_threads all_blocked_threads node_clock_list timers running_threads) nths_list);


(* Remove threads from list *)
fun remove_these_threads blocked_threads
						 [] = blocked_threads
  | remove_these_threads blocked_threads
						 (thread::other_unblocked_threads) = 
		(remove_these_threads (rm thread blocked_threads) other_unblocked_threads);

(* Update Blocked Threads - MAIN *)
fun update_blocked_threads running_threads
						   node_clock_list
						   timers
						   all_blocked_threads = 
			(remove_these_threads all_blocked_threads
							(find_all_removed_threads all_blocked_threads node_clock_list timers running_threads));

						 