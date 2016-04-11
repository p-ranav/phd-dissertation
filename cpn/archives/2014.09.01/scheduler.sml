(* Get the current partition name *)
fun get_current_part {clock_node=clock_node, clock_value=clock_value, schedule=[]} = 
						"SYSTEM"
  | get_current_part {clock_node=clock_node, clock_value=clock_value, 
						  schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::rest)} =
					part_name;

(* Get the CMQ_LIST for Partition X from the node-level cmqs*)
fun get_part_cmqs partition {cmq_node=cmq_node, cmqs=[]} = []
  | get_part_cmqs partition {cmq_node=cmq_node, cmqs=({cmq_part=cmq_part, cmq_list=cmq_list}::other_parts)} = 
            if (partition = cmq_part orelse cmq_part="SYSTEM") then 
            	(cmq_list)^^(get_part_cmqs partition {cmq_node=cmq_node, cmqs=other_parts})
            else get_part_cmqs partition {cmq_node=cmq_node, cmqs=other_parts};

(* Are all cmqs in Partition X on Node N EMPTY? If so, return true, else return false *)
fun cmq_empty_check [] = true
  | cmq_empty_check ({comp_name=comp_name, cmq=cmq}::other_ths) = 
        if (cmq <> []) then false else true andalso (cmq_empty_check other_ths);

(* Are all threads in Partition X on Node N passive? If so, return true, else return false*)
fun thread_passivity_check [] = true
  | thread_passivity_check ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, 
  					comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::other_ths) = 
           if (comp_op <> []) then false else true andalso (thread_passivity_check other_ths);

(* Get Candidate Threads for scheduling - Returns all threads in current Partition - Non exhaustive; FIX ME *)
fun get_candidates {clock_node=clock_node, clock_value=clock_value, schedule = []}
					{nid=nid, can_sched=can_sched, node_ths=[{part="SYSTEM", ths=ths}]} = ths
  | get_candidates {clock_node=clock_node, clock_value=clock_value, 
						  schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)}
				   {nid=nid, can_sched=can_sched, node_ths=[]} = []
  | get_candidates {clock_node=clock_node, clock_value=clock_value, 
						  schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)}
				   {nid=nid, can_sched=can_sched, node_ths={part=part, ths=ths}::other_part_ths} = 
			if (part_name = part orelse part="SYSTEM") then 
						(ths)^^(get_candidates 
								{clock_node=clock_node, clock_value=clock_value, 
						  			schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)}
						  		{nid=nid, can_sched=can_sched, node_ths=other_part_ths}) 
			else get_candidates {clock_node=clock_node, clock_value=clock_value, 
						  			schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)}
				   				 {nid=nid, can_sched=can_sched, node_ths=other_part_ths};

(* Check if there is any thread schedulable in THIS node - ADD can_sched = true if still valid *)
fun check_this_node node_clock node_cmq node_ths = 
	     if (((thread_passivity_check (get_candidates node_clock node_ths)) = false) 
		     orelse 
		    ((cmq_empty_check (get_part_cmqs (get_current_part node_clock) node_cmq)) = false))
		    then 
		    	true else false;

(* Check if there is any thread schedulable in any of the nodes *)
fun check_node_ths node_clock_list ncmq_list [] = false
  | check_node_ths node_clock_list ncmq_list 
			({nid=nid, can_sched=can_sched, node_ths=node_ths}::other_nodes) = 

		(check_this_node (hd (get_node_clock nid node_clock_list)) 
						 (hd (get_node_cmq nid ncmq_list))
						 {nid=nid, can_sched=can_sched, node_ths=node_ths})
				orelse (check_node_ths node_clock_list ncmq_list other_nodes);	


(* Main Schedule Guard *)
fun schedule_guard node_clock_list
				   ncmq_list
				   nths_list
				   timers
				   opn_list = 

		if ((timer_guard node_clock_list timers = false) andalso
			(enqueue_guard opn_list node_clock_list = false) andalso
			(check_node_ths node_clock_list ncmq_list nths_list = true))

			then true else false;

(* Get Thread CMQ *)
fun get_th_cmq comp ({comp_name=comp_name, cmq=cmq}::other_th) = 
     if (comp=comp_name) then cmq
     else (get_th_cmq comp other_th);

(* "Schedule Thread" after identifying candidates *)
fun sched_th [] {cmq_node=cmq_node, cmqs=cmqs}
	{clock_node=clock_node, clock_value=clock_value, schedule=schedule} = []
  | sched_th ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
				comp_st=comp_st, comp_op=comp_op}::other_ths)
			  {cmq_node=cmq_node, cmqs=cmqs}
			  {clock_node=clock_node, clock_value=clock_value, schedule=schedule} = 

		if (comp_op <> []) then [{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part,
								 comp_prio=comp_prio, comp_st=clock_value, comp_op=comp_op}]
		else 
			if ((get_th_cmq comp_name (get_part_cmqs comp_part {cmq_node=cmq_node, cmqs=cmqs})) <> [])
			then
				[{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio,
				comp_st=clock_value, comp_op=comp_op^^[hd (get_th_cmq comp_name (get_part_cmqs comp_part {cmq_node=cmq_node, cmqs=cmqs}))]}]
			else
				(sched_th other_ths {cmq_node=cmq_node, cmqs=cmqs} {clock_node=clock_node, clock_value=clock_value, schedule=schedule});

(* Find candidate threads and call sched_th *)
fun sched {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		  {cmq_node=cmq_node, cmqs=cmqs}
		  {nid=nid, can_sched=can_sched, node_ths=({part=part, ths=ths}::other_part_ths)} = 

		  (sched_th (get_candidates {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		  							{nid=nid, can_sched=can_sched, node_ths=({part=part, ths=ths}::other_part_ths)}) 
		  			{cmq_node=cmq_node, cmqs=cmqs} 
		  			{clock_node=clock_node, clock_value=clock_value, schedule=schedule});

(* Main Schedule Function *)
fun schedule_thread node_clock_list ncmq_list [] = []
  | schedule_thread node_clock_list 
					ncmq_list
					({nid=nid, can_sched=can_sched, node_ths=node_ths}::other_nodes) = 
		(sched (hd (get_node_clock nid node_clock_list))
			  (hd (get_node_cmq nid ncmq_list))
			  {nid=nid, can_sched=can_sched, node_ths=node_ths})^^
		(schedule_thread node_clock_list ncmq_list other_nodes);


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
           
[{cmq_node=cmq_node, cmqs = (update_cmq_list this_comp remove_opn {clock_node=clock_node, clock_value=clock_value, schedule=schedule} cmqs)}];

(* Main Node-specific dequeue function *)
fun dq_op {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		  {cmq_node=cmq_node, cmqs=cmqs}
		  {nid=nid, can_sched=can_sched, node_ths=({part=part, ths=ths}::other_part_ths)} = 

		  (update_node_cmq (#comp_name (hd (sched 
		  										{clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		  									  {cmq_node=cmq_node, cmqs=cmqs} 
		  									  {nid=nid, can_sched=can_sched, node_ths={part=part, ths=ths}::other_part_ths})))   

		  					(hd (#comp_op (hd (sched 
		  											{clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
		  										 {cmq_node=cmq_node, cmqs=cmqs} 
		  									     {nid=nid, can_sched=can_sched, node_ths={part=part, ths=ths}::other_part_ths})))) 
		      {clock_node=clock_node, clock_value=clock_value, schedule=schedule} {cmq_node=cmq_node, cmqs=cmqs});

(* Main dequeue function after scheduling a thread *)
fun dequeue_opn node_clock_list
				node_cmq_list
				[] = []
  | dequeue_opn node_clock_list
				node_cmq_list
				({nid=nid, can_sched=can_sched, node_ths=node_ths}::other_nodes) = 

		(dq_op (hd (get_node_clock nid node_clock_list))
			   (hd (get_node_cmq nid node_cmq_list))
			   {nid=nid, can_sched=can_sched, node_ths=node_ths})^^
			(dequeue_opn node_clock_list node_cmq_list other_nodes);


(* Update thread list after scheduling new thread*)
fun update_ths updated_ths {clock_node=clock_node, clock_value=clock_value, 
						  			schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)} ({part=part2, ths=ths}::other_part_ths) = 
           if (part_name=part2) then 
              ({part=part2, ths=updated_ths}::other_part_ths)
           else 
               {part=part2, ths=ths}::(update_ths updated_ths {clock_node=clock_node, clock_value=clock_value, 
						  			schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)} other_part_ths);

(* Update the node-level thread list after scheduling new thread *)
fun update_ready_ths updated_ths {clock_node=clock_node, clock_value=clock_value, schedule=schedule} {nid=nid, can_sched=can_sched, node_ths=node_ths} = 
         {nid=nid, can_sched=false, node_ths = (update_ths updated_ths {clock_node=clock_node, clock_value=clock_value, schedule=schedule} node_ths)};

(* Remove scheduled thread from thread list *)
fun remove_th this_comp ({comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::other_ths) = 
              if (this_comp=comp_name) then other_ths else
            {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}::(remove_th this_comp other_ths);

(* Main remove thread function *)
fun rm_th {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
		  					  {cmq_node=cmq_node, cmqs=cmqs} 
		  					  {nid=nid, can_sched=can_sched, node_ths={part=part, ths=ths}::other_part_ths} = 

		 (update_ready_ths (remove_th (#comp_name (hd (sched {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
									  					  {cmq_node=cmq_node, cmqs=cmqs} 
									  					  {nid=nid, can_sched=can_sched, node_ths={part=part, ths=ths}::other_part_ths}))) 			  
								  (get_candidates {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
							  					  {nid=nid, can_sched=can_sched, node_ths={part=part, ths=ths}::other_part_ths}))

		  					  {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
		  					  {nid=nid, can_sched=can_sched, node_ths={part=part, ths=ths}::other_part_ths})


(* Main dequeue function after scheduling a thread *)
fun remove_thread node_clock_list
				node_cmq_list
				[] = []
  | remove_thread node_clock_list
				node_cmq_list
				({nid=nid, can_sched=can_sched, node_ths=node_ths}::other_nodes) = 

		(rm_th (hd (get_node_clock nid node_clock_list))
			   (hd (get_node_cmq nid node_cmq_list))
			   {nid=nid, can_sched=can_sched, node_ths=node_ths})::
			(remove_thread node_clock_list node_cmq_list other_nodes);