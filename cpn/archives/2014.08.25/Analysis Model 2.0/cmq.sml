(* Enqueue Guard - Make sure the partition is active before enqueuing *)
fun opnq_check {clock_node=clock_node, clock_value=clock_value, 
				 	schedule=[]} 
				{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,  
				op_calls=op_calls}
				{cmq_node=cmq_node, cmqs=cmqs} = 
		if ((clock_node = op_node) andalso (op_node = cmq_node) andalso (clock_value < clock_limit)) then true else false
  | opnq_check {clock_node=clock_node, clock_value=clock_value, 
				 	schedule=({part_name=part_name, exec_t=exec_t, dur=dur, pr=pr, off=off}::other_parts)} 
				{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, 
				op_calls=op_calls}
				{cmq_node=cmq_node, cmqs=cmqs} = 
		if ((clock_node = op_node) andalso (op_node = cmq_node) andalso (part_name = op_pn) andalso (clock_value < clock_limit)) then true else false;

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