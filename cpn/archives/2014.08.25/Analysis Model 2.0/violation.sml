(* Main Violation Check - NEED TO ADD OP_DV to make sure that violation transition doesnt constantly fire - so clock_value - op_st > op_dl andalso op_dv = false *)
fun violation_check {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
					{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = false
  | violation_check {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
  					{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,
  						op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

  				if (clock_value - op_st > op_dl andalso op_dv = false) then true else false;


(* dv_guard *)
fun dv_guard {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
			 {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = 

			 if ((clock_node = comp_node) andalso 
			 	 (violation_check {clock_node=clock_node, clock_value=clock_value, schedule=schedule} 
			 					  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = true))

			 					  then true else false;

(* Handle Deadline Violation on thread -- FIX ME!! ADD op_dv and fix me please *) 
fun handle_dv {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,
  						op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

  						{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=true,
  						op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};

(* Get_dv_op *)
fun get_dv_op {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = comp_op;