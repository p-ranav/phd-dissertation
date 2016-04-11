(* GLOBAL CONSTANTS *)

val time_res = 1;
val clock_tick = 4;
(*---------------------------------------------------------------------------------------------------------*)

(* Get the current partition name *)
fun get_current_part {clock_node=clock_node, clock_value=clock_value, 
						  schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::rest)} =
					part_name;

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
fun progress_schedule x {clock_node=clock_node, clock_value=clock_value, 
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

(*---------------------------------------------------------------------------------------------------------*)				       					
(* Make Sure that the node clock, cmq and thread tokens are from the same computing node *)
fun node_check {clock_node=clock_node, clock_value=clock_value, schedule = schedule}
			   {cmq_node=cmq_node, cmqs=cmqs}
			   {nid=nid, node_ths=node_ths} = 
		if (clock_node = cmq_node andalso cmq_node = nid) then true else false;


(* Get the CMQ_LIST for Partition X from the node-level cmqs*)
fun get_part_cmqs partition {cmq_node=cmq_node, cmqs=[]} = []
  | get_part_cmqs partition {cmq_node=cmq_node, cmqs=({cmq_part=cmq_part, cmq_list=cmq_list}::other_parts)} = 
            if (partition = cmq_part) then cmq_list
            else get_part_cmqs partition {cmq_node=cmq_node, cmqs=other_parts};

(* Need to check CMQs of all system threads too *)
fun get_system_cmqs {cmq_node=cmq_node, cmqs=[]} = []
  | get_system_cmqs {cmq_node=cmq_node, cmqs=({cmq_part=cmq_part, cmq_list=cmq_list}::other_parts)} = 
  			if (cmq_part = "SYSTEM") then cmq_list
  			else get_system_cmqs {cmq_node=cmq_node, cmqs=other_parts};

(* Get Thread CMQ *)
fun get_th_cmq comp ({comp_name=comp_name, cmq=cmq}::other_th) = 
     if (comp=comp_name) then cmq
     else (get_th_cmq comp other_th);

(* Are all cmqs in Partition X on Node N EMPTY? If so, return true, else return false *)
fun cmq_empty_check [] = true
  | cmq_empty_check ({comp_name=comp_name, cmq=cmq}::other_ths) = 
        if (cmq <> []) then false else true andalso (cmq_empty_check other_ths);

(* Are all threads in Partition X on Node N passive? If so, return true, else return false*)
fun thread_passivity_check [] = true
  | thread_passivity_check ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, 
  					comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::other_ths) = 
           if (comp_op <> []) then false else true andalso (thread_passivity_check other_ths);

(* Get Candidate Threads for scheduling - Returns all threads in current Partition *)
fun get_candidates {clock_node=clock_node, clock_value=clock_value, 
						  schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)}
				   {nid=nid, node_ths={part=part, ths=ths}::other_part_ths} = 
			if (part_name = part) then ths 
			else get_candidates {clock_node=clock_node, clock_value=clock_value, 
						  			schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)}
				   				 {nid=nid, node_ths=other_part_ths};


(* Find all SYSTEM partition threads first - include them in the scheduling process *)
fun find_system_threads {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
						{nid=nid, node_ths=[]} = []
| find_system_threads   {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
						{nid=nid, node_ths={part=part, ths=ths}::other_part_ths} = 
				if ((clock_node = nid) andalso (part="SYSTEM")) then ths
				else 
					find_system_threads {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
										{nid=nid, node_ths=other_part_ths};

(* THE Schedule Guard! *)
fun schedule_guard node_clock node_cmq node_threads = 
	if  (   (node_check node_clock node_cmq node_threads = true) andalso 
		   ((thread_passivity_check ((find_system_threads node_clock node_threads)^^(get_candidates node_clock node_threads)) = false) 
		     orelse 
		    (cmq_empty_check ((get_system_cmqs node_cmq)^^(get_part_cmqs (get_current_part node_clock) node_cmq)) = false))) 
		then true 
		else false;

(* "Schedule Thread" after identifying candidates *)
fun sched_th ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
				comp_st=comp_st, comp_op=comp_op}::other_ths)
			  {cmq_node=cmq_node, cmqs=cmqs}
			  {clock_node=clock_node, clock_value=clock_value, schedule=schedule} = 

		if (comp_op <> []) then {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part,
								 comp_prio=comp_prio, comp_st=clock_value, comp_op=comp_op}
		else 
			if ((get_th_cmq comp_name (get_part_cmqs comp_part {cmq_node=cmq_node, cmqs=cmqs})) <> [])
			then
				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
				comp_st=clock_value, comp_op=comp_op^^[hd (get_th_cmq comp_name (get_part_cmqs comp_part {cmq_node=cmq_node, cmqs=cmqs}))]}
			else
				(sched_th other_ths {cmq_node=cmq_node, cmqs=cmqs} {clock_node=clock_node, clock_value=clock_value, schedule=schedule});

(* Find candidate threads and call sched_th *)
fun sched {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		  {cmq_node=cmq_node, cmqs=cmqs}
		  {nid=nid, node_ths={part=part, ths=ths}::other_part_ths} = 

		  (sched_th (get_candidates {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		  							{nid=nid, node_ths={part=part, ths=ths}::other_part_ths}) 
		  			{cmq_node=cmq_node, cmqs=cmqs} 
		  			{clock_node=clock_node, clock_value=clock_value, schedule=schedule});

(* Remove the operation! *)
fun remove_cmq_opn this_comp remove_opn ({comp_name=comp_name, cmq=cmq}::other_ths) = 
         if (comp_name = this_comp) then ({comp_name=comp_name, cmq=(rm remove_opn cmq)}::other_ths)
        else {comp_name=comp_name, cmq=cmq}::(remove_cmq_opn this_comp remove_opn other_ths);

(* Find the part in which the cmq resides *)
fun update_cmq_list this_comp remove_opn {clock_node=clock_node, clock_value=clock_value, 
						  			schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)} 

						  		({cmq_part=cmq_part, cmq_list=cmq_list}::other_part) = 

          if (part_name=cmq_part) then ({cmq_part=cmq_part, cmq_list = (remove_cmq_opn this_comp remove_opn cmq_list)}::other_part) 
          else {cmq_part=cmq_part, cmq_list=cmq_list}::(update_cmq_list this_comp remove_opn {clock_node=clock_node, clock_value=clock_value, 
						  			schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)} other_part);


(* Update Node CMQ after dequeuing the operation *)
fun update_node_cmq this_comp remove_opn {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
							 	 	{cmq_node=cmq_node, cmqs=cmqs} =
           
{cmq_node=cmq_node, cmqs = (update_cmq_list this_comp remove_opn {clock_node=clock_node, clock_value=clock_value, schedule=schedule} cmqs)};

(* Main dequeue function *)
fun dq_op {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		  {cmq_node=cmq_node, cmqs=cmqs}
		  {nid=nid, node_ths={part=part, ths=ths}::other_part_ths} = 

		  (update_node_cmq (#comp_name (sched {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		  									  {cmq_node=cmq_node, cmqs=cmqs} 
		  									  {nid=nid, node_ths={part=part, ths=ths}::other_part_ths}))   

		  					(hd (#comp_op (sched {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
		  										 {cmq_node=cmq_node, cmqs=cmqs} 
		  									     {nid=nid, node_ths={part=part, ths=ths}::other_part_ths}))) 
		      {clock_node=clock_node, clock_value=clock_value, schedule=schedule} {cmq_node=cmq_node, cmqs=cmqs});


(* Update thread list after scheduling new thread*)
fun update_ths updated_ths {clock_node=clock_node, clock_value=clock_value, 
						  			schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)} ({part=part2, ths=ths}::other_part_ths) = 
           if (part_name=part2) then 
              ({part=part2, ths=updated_ths}::other_part_ths)
           else 
               {part=part2, ths=ths}::(update_ths updated_ths {clock_node=clock_node, clock_value=clock_value, 
						  			schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)} other_part_ths);

(* Update the node-level thread list after scheduling new thread *)
fun update_ready_ths updated_ths {clock_node=clock_node, clock_value=clock_value, schedule=schedule} {nid=nid, node_ths=node_ths} = 
         {nid=nid, node_ths = (update_ths updated_ths {clock_node=clock_node, clock_value=clock_value, schedule=schedule} node_ths)};

(* Remove scheduled thread from thread list *)
fun remove_th this_comp ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::other_ths) = 
              if (this_comp=comp_name) then other_ths else
            {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::(remove_th this_comp other_ths);

(* Main remove thread function *)
fun rm_th {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		  					  {cmq_node=cmq_node, cmqs=cmqs} 
		  					  {nid=nid, node_ths={part=part, ths=ths}::other_part_ths} = 

		 (update_ready_ths (remove_th (#comp_name (sched {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
									  					  {cmq_node=cmq_node, cmqs=cmqs} 
									  					  {nid=nid, node_ths={part=part, ths=ths}::other_part_ths})) 			  
								  (get_candidates {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
							  					  {nid=nid, node_ths={part=part, ths=ths}::other_part_ths}))

		  					  {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
		  					  {nid=nid, node_ths={part=part, ths=ths}::other_part_ths})

(*---------------------------------------------------------------------------------------------------------*)

(* Guard for Timer Expiry *)
fun timer_check {t_nid=t_nid, t_comp=t_comp, t_pn=t_pn, t_pr=t_pr, t_off=t_off, t_op=t_op}
				{clock_node=clock_node, clock_value=clock_value, schedule=schedule} = 
			(* Node check + offset check *)
			if (t_nid = clock_node andalso t_off = clock_value) then true else false;	

(* Upate timer offset after expiry *)
fun update_timer {t_nid=t_nid, t_comp=t_comp, t_pn=t_pn, t_pr=t_pr, t_off=t_off, t_op=t_op} = 
			if (t_pr <> 0) then 
				 1`{t_nid=t_nid, t_comp=t_comp, t_pn=t_pn, t_pr=t_pr, t_off=t_off+t_pr, t_op=t_op}		   	
			else empty;	 

(* Get timer operation *)
fun get_t_op {t_nid=t_nid, t_comp=t_comp, t_pn=t_pn, t_pr=t_pr, t_off=t_off, t_op=t_op} = t_op;

(*---------------------------------------------------------------------------------------------------------*)

(* Enqueue Guard - Make sure the partition is active before enqueuing *)
fun opnq_check {clock_node=clock_node, clock_value=clock_value, 
				 	schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_parts)} 
				{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv, 
				op_calls=op_calls}
				{cmq_node=cmq_node, cmqs=cmqs} = 
		if ((clock_node = op_node) andalso (op_node = cmq_node) andalso (part_name = op_pn)) then true else false;


(* Actual Enqueue *)
fun nq_this {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, 			  
			op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=op_calls} 
			 [] = [{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
				op_calls=op_calls}]
  | nq_this {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
				op_calls=op_calls} 

			({opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, op_st=op_st2, op_et=op_et2, op_exec_t=op_exec_t2, op_dv=op_dv2,  
				op_calls=op_calls2}::other_ops2) = 

			if (op_prio > op_prio2) then
				({opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
				op_calls=op_calls}::
				 {opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, op_st=op_st2, op_et=op_et2, op_exec_t=op_exec_t2, op_dv=op_dv2,  
				op_calls=op_calls2}::other_ops2)
			else 
				{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, op_st=op_st2, op_et=op_et2, op_exec_t=op_exec_t2, op_dv=op_dv2,  
				op_calls=op_calls2}::
				(nq_this {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, 			  op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,    op_calls=op_calls} other_ops2 );

(* Find CMQ to enqueue on *)
fun find_cmq ({comp_name=comp_name, cmq=cmq}::other_ths) {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
				op_calls=op_calls} = 
			if (comp_name = op_comp) then 
					({comp_name=comp_name, cmq = (nq_this {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
				op_calls=op_calls} cmq)}::other_ths)
			else 
				{comp_name=comp_name, cmq=cmq}::(find_cmq other_ths {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   
				op_calls=op_calls});


(* Find the right part-cmq before actual enqueue *)
fun find_part_cmq ({cmq_part=cmq_part, cmq_list=cmq_list}::other_part)
				  {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, 
				   op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
				   op_calls=op_calls} = 
			if (cmq_part = op_pn) then 
				 {cmq_part=cmq_part, cmq_list = (find_cmq cmq_list {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
				op_calls=op_calls})}::other_part
			else 
				({cmq_part=cmq_part, cmq_list=cmq_list}::(find_part_cmq other_part {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
				op_calls=op_calls}));

(* Enqueue Operation onto CMQ *)
fun opnq {clock_node=clock_node, clock_value=clock_value, 
				 	schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_parts)}
		 {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
				op_calls=op_calls}
		 {cmq_node=cmq_node, cmqs=cmqs} = 

		 {cmq_node=cmq_node, cmqs = ( find_part_cmq cmqs {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=clock_value, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
				op_calls=op_calls} )};



(*---------------------------------------------------------------------------------------------------------*)

(* Can the currently running thread "execute" - is there anything TO execute; is the call complete?; is the op complete? - check all*)
fun can_execute_th {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = false
  | can_execute_th {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et,
			    	op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=[]}]} = false
  | can_execute_th {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, 
  					op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
                if (call_exec_t < call_dur) then true else false;

(* Main Execute Guard! *)
fun execute_guard {clock_node=clock_node, clock_value=clock_value, 
				 	schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_parts)}
				  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = 
	if ((clock_node = comp_node) andalso (part_name = comp_part) andalso (clock_value - comp_st < clock_tick) 
	     andalso ((can_execute_th {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}) = true))
	     then true else false; 


(* Handle a DDS Publish! *)
fun handle_dds_publish {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t + 1, op_dv=op_dv,    op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + 1, call_dur=call_dur}::other_calls)}]};


(* Handle a DDS Subscribe! *)
fun handle_dds_subscribe {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t + 1, op_dv=op_dv,    op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + 1, call_dur=call_dur}::other_calls)}]};

(* Local piece of code taking up CPU! i.e. on a "Local" port *)
fun handle_code {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  					 op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, 									call_dur=call_dur}::other_calls)}]} = 

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t + 1, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + 1, call_dur=call_dur}::other_calls)}]};


(* Handle incoming call on Facet - Server side of an RMI or AMI *)
fun handle_facet {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  
					   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t + 1, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + 1, call_dur=call_dur}::other_calls)}]};


(* Handle an out call on a receptacle - if rmi, block! else dont worry *)
fun handle_receptacle_outcall {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, 
			                   															op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
			        		if (call_exec_t < q_t) then 
			        				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, 
			                   															op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t + time_res, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + time_res, call_dur=call_dur}::other_calls)}]}
			                else 
			                	if (call_exec_t > q_t andalso call_exec_t < call_dur) then
			                		{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, 
			                   															op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t + time_res, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + time_res, call_dur=call_dur}::other_calls)}]}
			                   	else 
			                		{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, 
			                   															op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};			                   		


fun execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

			   case port_type of "Publisher" => handle_dds_publish {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, 																								op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, 																					unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

			                   | "Subscriber" => handle_dds_subscribe {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
			                   											op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

			                   | "Local" => handle_code {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
			                   											op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

			                   | "Facet" => handle_facet {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
			                   											op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

			                   | "Receptacle" => handle_receptacle_outcall {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, 
			                   															op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,  op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};



(* Is it time to block the running thread? - i.e. Did I just push out an RMI call? *)	
fun isRMI {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
		if ((blk_on_complete = true) andalso (port_type = "Receptacle") andalso (call_exec_t = q_t)) then true else false;  


(* Use the above call and update thread "state" *)
fun update_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,    op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
              if (isRMI(execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}) = false) 

              then 1`(execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]})

              else empty;            

(* Block Thread because RMI! *)
fun block_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

              if (isRMI (execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}) = true) then

              1`(execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,    op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]})

              else empty;	


(* Is it time to record an AMI call to bring it back later  as an operation? - i.e. Did I just push out an AMI call? *)
fun isAMI  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
		if ((blk_on_complete = false) andalso (port_type = "Receptacle") andalso (call_exec_t = q_t)) then true else false;  

(* Block Thread because RMI! *)
fun record_ami {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

              if (isAMI (execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}) = true) then

              1`{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=[{port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=true, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t+time_res, call_dur=call_dur}]}

              else empty;	

(* Time to check if executing my running thread induces an operation on some other thread on some other node! *)
fun canInduce {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name1, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}
                    {node_name=this_node, port_name=this_port, opn=opn} = 

         (* In case of RMI or AMI *)
         if((comp_node = this_node) andalso (port_name1=this_port) andalso (port_type = "Receptacle") andalso (call_exec_t = q_t) andalso (induction = false))	then
         			true 
         else

         	(* In case of Push Subscription! *)
         	if ((port_type = "Publisher") andalso ((comp_node = this_node) orelse (this_node = "ALL")) andalso (port_name1=this_port) andalso (call_exec_t = call_dur) andalso (induction = false)) then true
         	
         	else false;	

(* Main Induce function - Do the check and return an operation that needs to be enqueued *)		
fun induce_th {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st,
					 op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name1, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}
              {node_name=this_node, port_name=this_port, opn=opn} = 

            (* Check induction after executing! - on executing, if call_exec_t becomes equal to call_dur or q_t, then simultaneously induce operation on another component! *)
            if (canInduce 
            			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, 
            							op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name1, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

                    {node_name = this_node, port_name=this_port, opn=opn} = true) then 

                    	1`(opn)

            else empty;	


(* Handle Induce! Set induction to true to let the simulator know that induciton just happened *)
fun handle_induce {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st,
					 op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name1, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} =

				 {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st,
					 op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name1, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=true, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}



(*---------------------------------------------------------------------------------------------------------*)

(* Check thread exec_t - has thread run for a clock tick? - the min time assigned to it when it was scheduled *)
fun preempt_check clock_value {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} =
    (clock_value - comp_st = clock_tick);


(* Main Preempt Guard! *)
fun preempt_guard {clock_node=clock_node, clock_value=clock_value, 
				 	schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_parts)}
				  {nid=nid, node_ths=node_ths}
				  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = 

			if ((clock_node = comp_node) andalso (nid=comp_node) andalso (preempt_check clock_value {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = true)) 
			then true else false;	                   											

(* House keeping - Remove op if needed on preempt *)
fun remove_op {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = 
                          {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]}
| remove_op {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et,
			    	op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=[]}]} =
                     {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]}
| remove_op {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, 
  					op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
            if(call_exec_t = call_dur andalso other_calls = []) then 
              {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} 
           else 
           	   if (call_exec_t = call_dur) then 
           	   		{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=other_calls}]}
           	   else
           	        {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, 
  					op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};


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


(* Call this enqueue function when you need to return a thread back to the thread queue *)
fun enqueue_thread {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} 
                	({part=part, ths=ths}::rest) = 
       if (comp_part = part) then
           ({part=part, ths = (pnq_helper {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} ths)}::rest)
       else
           ({part=part, ths=ths}::(enqueue_thread {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} rest));


(* Main Preempt Function *)
fun preempt_th thread {nid=nid, node_ths=node_ths} =
   {nid=nid, node_ths=(enqueue_thread (remove_op thread) node_ths)};   



(*---------------------------------------------------------------------------------------------------------*)

(* Is it time to unblock an RMI-blocked thread? Lets see - we need a call on a facet to complete; and we need to unblock the right thread - i.e. the thread to be unblocked should be in its unblk_list! *)
fun canUnblock {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[]}

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, 
  					op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type="Receptacle", port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=true, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

  			{nid=nid, node_ths=node_ths} = false

 | canUnblock {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 				op_st=op_st2, op_et=op_et2, op_exec_t=op_exec_t2, op_dv=op_dv2,   op_calls=({port_type=port_type2, port_name=port_name2, call_op_name=call_op_name2, unblk_list=unblk_list2, blk_on_complete=blk_on_complete2, induction=induction2, q_t=q_t2, pr_t=pr_t2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]}

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, 
  					op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type="Receptacle", port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=true, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} 

  			{nid=nid, node_ths=node_ths} =

        if ((other_calls2 = []) andalso (contains unblk_list2 [{node_name=comp_node, comp_name=comp_name, port_name=port_name}]) = true andalso (call_exec_t2  = call_dur2) andalso (call_exec_t = q_t) andalso (nid = comp_node) ) then true else false;

(* Update Exec Time after unblocking! *)
fun update_exec_t {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 				op_st=op_st2, op_et=op_et2, op_exec_t=op_exec_t2, op_dv=op_dv2,   op_calls=({port_type="Facet", port_name=port_name2, call_op_name=call_op_name2, unblk_list=unblk_list2, blk_on_complete=blk_on_complete2, induction=induction2, q_t=q_t2, pr_t=pr_t2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]}

					{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, 
  					op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type="Receptacle", port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=true, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}  = 


  				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t + call_exec_t2, op_dv=op_dv,   op_calls=({port_type="Receptacle", port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=true, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + call_exec_t2, call_dur=call_dur + call_exec_t2}::other_calls)}]};

(* Update AMI exec_t *)
fun update_ami_exec_t {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 						op_st=op_st2, op_et=op_et2, op_exec_t=op_exec_t2, op_dv=op_dv2,   op_calls=({port_type="Facet", port_name=port_name2, call_op_name=call_op_name2, unblk_list=unblk_list2, blk_on_complete=blk_on_complete2, induction=induction2, q_t=q_t2, pr_t=pr_t2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls2)}]}

 					   {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, 
	  					op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=[{port_type="Receptacle", port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}]} = 

	  			{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, 
	  					op_exec_t=op_exec_t+op_exec_t2, op_dv=op_dv,   op_calls=[{port_type="Receptacle", port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t+op_exec_t2, call_dur=call_dur+op_exec_t2+pr_t}]};

(* Main unblock thread function *)
fun unblock_th thread blocked_thread {nid=nid, node_ths=node_ths} =
   {nid=nid, node_ths=(enqueue_thread (update_exec_t thread blocked_thread) node_ths)}; 


(* Unblock AMI operation the same way; Did the currently running thread complete an AMI on server side *)
fun isAMIcomplete {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[]}

				{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, 
	  					op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=[{port_type="Receptacle", port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=true, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}]} = false

  | isAMIcomplete {comp_node=comp_node2, comp_name=comp_name2, comp_part=comp_part2, comp_prio=comp_prio2, comp_st=comp_st2, comp_op=[{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, 
 					op_st=op_st2, op_et=op_et2, op_exec_t=op_exec_t2, op_dv=op_dv2,   op_calls=({port_type=port_type2, port_name=port_name2, call_op_name=call_op_name2, unblk_list=unblk_list2, blk_on_complete=blk_on_complete2, induction=induction2, q_t=q_t2, pr_t=pr_t2, call_st=call_st2, call_et=call_et2, call_exec_t=call_exec_t2, call_dur=call_dur2}::other_calls)}]}

 				  {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, 
	  					op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=[{port_type="Receptacle", port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}]} =

	  			(* Check to make sure its an AMI server *)
	  			if ((other_calls = []) andalso ((contains unblk_list2 [{node_name=op_node, comp_name=op_comp, port_name=port_name}]) = true) andalso (call_exec_t2 = call_dur2)) then true else false;

(*---------------------------------------------------------------------------------------------------------*)


(* Is some call complete? - should I remove this call from the op_call_list? *)
fun call_check {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = false
  | call_check {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, 		
  				op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=[]}]} = false
  | call_check {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, 
  				op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

  					  (* AMI call complete *)
  					  if ((port_type="Receptacle") andalso (call_exec_t = q_t) andalso (blk_on_complete = false) andalso (call_exec_t = call_dur)) then true
  					  else 
                      		if((call_exec_t = call_dur)) then true else false;


(* Is the entire op complete? - can I remove the op and say mark it as complete? *)
fun handle_call_complete {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, 
  				op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

  		(* Case1: Call + whole op is complete! *)
  		if (call_exec_t = call_dur andalso other_calls = []) then 
  				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]}

  			 (* Case2: Call is complete but op is NOT complete *)
  		else 
  			if (call_exec_t = call_dur andalso other_calls <> []) then
  				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio,
    										 op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=other_calls}]}
    		 else 
    		 	{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, 
  				op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};


(* Update call list main function *)
fun update_call_list {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

    				(handle_call_complete {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio,
    										 op_dl=op_dl, op_st=op_st, op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]});

(*---------------------------------------------------------------------------------------------------------*)


(* Record a completed operation! *)
fun is_op_complete {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = false
| is_op_complete   {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, 
  						op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

  						if (call_exec_t = call_dur andalso other_calls = []) then true else false;

(* Get completed operation *)
fun get_completed_op {clock_node=clock_node, clock_value=clock_value, schedule=schedule}	
       				 {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, 
  						op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

  				1`{node=comp_node, op_name=opname, op_st=op_st, op_et=clock_value};

(* Record Completed Operations! Main Function! *)
fun record_cop clock
			   thread = 
	if (is_op_complete thread) then 
		(get_completed_op clock thread)
	else empty;


(*---------------------------------------------------------------------------------------------------------*)

(* Main Violation Check *)
fun violation_check {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
					{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = false
  | violation_check {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
  					{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, 
  						op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

  				if (clock_value - op_st > op_dl andalso op_dv=false) then true else false;


(* dv_guard *)
fun dv_guard {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
			 {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = 

			 if ((clock_node = comp_node) andalso 
			 	 (violation_check {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
			 					  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = true))

			 					  then true else false;

(* Handle Deadline Violation on thread *) 
fun handle_dv {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, 
  						op_et=op_et, op_exec_t=op_exec_t, op_dv=op_dv,   op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

  						{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, 
  						op_et=op_et, op_exec_t=op_exec_t, op_dv=true,  op_calls=({port_type=port_type, port_name=port_name, call_op_name=call_op_name, unblk_list=unblk_list, blk_on_complete=blk_on_complete, induction=induction, q_t=q_t, pr_t=pr_t, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};

(* Get_dv_op *)
fun get_dv_op {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = comp_op;








