(* Is some call complete - should I remove this call from the op_call_list *)
fun call_check {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = false

 |  call_check {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,  		
  				op_calls=[]}]} = false

 |  call_check {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, 
  				op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

  					  if (call_exec_t = call_dur) then 
                    true 
              else 
                    false;


(* Is the entire op complete - can I remove the op and say mark it as complete *)
fun handle_call_complete {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,
  				op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

  		(* Case1 Call + whole op is complete! *)
  		if (call_exec_t = call_dur andalso other_calls = []) then 
  				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]}

  			 (* Case2 Call is complete but op is NOT complete *)
  		else 
  			if (call_exec_t = call_dur andalso other_calls <> []) then
  				{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio,
    										 op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=other_calls}]}
    		 else 
    		 	{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,
  				op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};


(* Update call list main function *)
fun update_call_list {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

    				(handle_call_complete {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio,
    										 op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]});

(* Record a completed operation! *)
fun is_op_complete {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = false
| is_op_complete   {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,
              op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

              if (call_exec_t = call_dur andalso other_calls = []) then true else false;

(* Get completed operation *)
fun get_completed_op {clock_node=clock_node, clock_value=clock_value, schedule=schedule}  
               {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,
              op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
          1`{node=comp_node, op_name=opname, op_st=op_st, op_et=clock_value};

(* Record Completed Operations! Main Function! *)
fun record_cop clock
         thread = 
  if (is_op_complete thread) then 
    (get_completed_op clock thread)
  else empty;