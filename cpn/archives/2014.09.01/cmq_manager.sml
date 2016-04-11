(* Can_Enqueue - Given an operation and a node_clock - Can I enqueue this operation right now? *)
(* If schedule = [], all threads are system threads, so you can enqueue right now *)
(* If schedule <> [], then check if the operation's partition is active; If so, you can enqueue *)
fun can_enqueue {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls}
				{clock_node=clock_node, clock_value=clock_value, schedule=[]} = 
		if (op_node = clock_node) then true else false
  | can_enqueue {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls}
                {clock_node=clock_node, clock_value=clock_value, 
                	schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_pns)} = 
         if (op_node=clock_node andalso op_pn=part_name) then true else false;


(* Operation Enqueue guard *)
fun enqueue_guard [] node_clock_list
                  = false
  | enqueue_guard ({opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls}::other_opns) 
                    node_clock_list = 
        if (can_enqueue {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls} 
                         (hd (get_node_clock op_node node_clock_list)) 
        					= true) then true
        else 
        	(enqueue_guard other_opns node_clock_list);


(* Actual Enqueue *)
fun nq_this {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, 			  
			op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls} 
			 [] = [{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,  
				op_calls=op_calls}]
  | nq_this {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, 
				op_calls=op_calls} 

			({opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, op_st=op_st2, op_dv=op_dv2, 
				op_calls=op_calls2}::other_ops2) = 

			if (op_prio > op_prio2) then
				({opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,   
				op_calls=op_calls}::
				 {opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, op_st=op_st2, op_dv=op_dv2, op_calls=op_calls2}::other_ops2)
			else 
				{opname=opname2, op_node=op_node2, op_comp=op_comp2, op_pn=op_pn2, op_prio=op_prio2, op_dl=op_dl2, op_st=op_st2, op_dv=op_dv2, op_calls=op_calls2}::
				(nq_this {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls} other_ops2 );

(* Find CMQ to enqueue on - CHECK BASE CASE What if CMQ isnt found? *)
fun find_cmq ({comp_name=comp_name, cmq=cmq}::other_ths) {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls} = 
			if (comp_name = op_comp) then 
					({comp_name=comp_name, cmq = (nq_this {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls} cmq)}::other_ths)
			else 
				{comp_name=comp_name, cmq=cmq}::(find_cmq other_ths {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls});


(* Find the right part-cmq before actual enqueue  - CHECK BASE CASE What if Partition isnt found? *)
fun find_part_cmq ({cmq_part=cmq_part, cmq_list=cmq_list}::other_part)
				  {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, 
				   op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,  
				   op_calls=op_calls} = 
			if (cmq_part = op_pn) then 
				 {cmq_part=cmq_part, cmq_list = (find_cmq cmq_list {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, 
				op_calls=op_calls})}::other_part
			else 
				({cmq_part=cmq_part, cmq_list=cmq_list}::(find_part_cmq other_part {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls}));

(* Enqueue Operation onto CMQ *)
fun opnq {clock_node=clock_node, clock_value=clock_value, 
				 	schedule=schedule}
		 {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls}
		 {cmq_node=cmq_node, cmqs=cmqs} = 

		 {cmq_node=cmq_node, cmqs = ( find_part_cmq cmqs {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=clock_value, op_dv=op_dv, op_calls=op_calls} )};				  

(* Find the right node cmq before enqueue *)
fun find_node_cmq  node_clock_list
					{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls}
				   ({cmq_node=cmq_node, cmqs=cmqs}::other_node_cmqs) =

			if (op_node = cmq_node) then 
					((opnq (hd (get_node_clock op_node node_clock_list))
						  {opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls}
						  {cmq_node=cmq_node, cmqs=cmqs})::other_node_cmqs)
			else 
				({cmq_node = cmq_node, cmqs=cmqs}::
				(find_node_cmq node_clock_list
					{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=op_calls}
					other_node_cmqs));


(* Main Enqueue Operation Function *)
fun enqueue_opns node_clock_list [] node_cmq_list = node_cmq_list
  | enqueue_opns node_clock_list
				 (first_op::rest_opns)
				 node_cmq_list = 
		if (enqueue_guard [first_op] node_clock_list = true) then 
			 (enqueue_opns node_clock_list rest_opns (find_node_cmq node_clock_list first_op node_cmq_list))
		else 
			(enqueue_opns node_clock_list rest_opns node_cmq_list);


(* Find operations that need to be removed from Waiting List *)
fun update_waiting_list node_clock_list [] node_cmq_list = []
  | update_waiting_list node_clock_list
  						  (first_op::rest_opns)
  						  node_cmq_list = 
  		if (enqueue_guard [first_op] node_clock_list = true) then 
  				(update_waiting_list node_clock_list rest_opns node_cmq_list)
  		else 
  			(first_op)::(update_waiting_list node_clock_list rest_opns node_cmq_list)