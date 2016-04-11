(* Can the currently running thread "execute" - is there anything TO execute; is the call complete?; is the op complete? - check all*)
fun can_execute_th {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[]} = false
  | can_execute_th {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=[]}]} = false
  | can_execute_th {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
                if (call_exec_t < call_dur) then true else false;

(* Main Execute Guard! *)
fun execute_guard {clock_node=clock_node, clock_value=clock_value, 
				 	schedule=schedule}
				  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op} = 
	if ((clock_node = comp_node) andalso (clock_value - comp_st < clock_tick) 
	     andalso ((can_execute_th {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=comp_op}) = true))
	     then true else false; 


(* Handle a DDS Publish! *)
fun handle_dds_publish {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + 1, call_dur=call_dur}::other_calls)}]};


(* Handle a DDS Subscribe! *)
fun handle_dds_subscribe {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + 1, call_dur=call_dur}::other_calls)}]};

(* Local piece of code taking up CPU! i.e. on a "Local" port *)
fun handle_code {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + 1, call_dur=call_dur}::other_calls)}]};


(* Handle incoming call on Facet - Server side of an RMI or AMI *)
fun handle_facet {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,
					   op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

			{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t + 1, call_dur=call_dur}::other_calls)}]};


(* Handle an out call on a receptacle - if rmi, block! else dont worry *)
fun handle_receptacle_outcall {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, 
			                   															op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,
			                   															op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

			                		{comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, 
			                   															op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};			                   		


fun execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

			   case port_type of "Publisher" => handle_dds_publish {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, 																								op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  																					unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

			                   | "Subscriber" => handle_dds_subscribe {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
			                   											op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

			                   | "Local" => handle_code {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
			                   											op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

			                   | "Facet" => handle_facet {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, 
			                   											op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

			                   | "RMI_Receptacle" => handle_receptacle_outcall {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, 
			                   															op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}

			                   | "AMI_Receptacle" => handle_receptacle_outcall {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, 
			                   															op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]};			                   											



(* Is it time to block the running thread? - i.e. Did I just push out an RMI call? *)	
fun isRMI {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
		if (port_type = "RMI_Receptacle") then true else false;  


(* Use the above call and update thread "state" *)
fun update_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
              if (isRMI(execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,  op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}) = false) 

              then 1`(execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]})

              else empty;            

(* Block Thread because RMI! *)
fun block_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 

              if (isRMI (execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}) = true) then

              1`(execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv, op_calls=({port_type=port_type, port_name=port_name,  unblk_list=unblk_list, induced=induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]})

              else empty;


(* Will the execution of this thread block the thread? *)
fun will_block  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, 
                           comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,  
                              op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, 
                                                induced =induced, call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]} = 
              if (isRMI (execute_th  {comp_node=comp_node, comp_name=comp_name, comp_part=comp_part, comp_prio=comp_prio, comp_st=comp_st, 
                                                 comp_op=[{opname=opname, op_node=op_node, op_comp=op_comp, op_pn=op_pn, op_prio=op_prio, op_dl=op_dl, op_st=op_st, op_dv=op_dv,   
                                                 op_calls=({port_type=port_type, port_name=port_name, unblk_list=unblk_list, induced =induced, 
                                                                  call_st=call_st, call_et=call_et, call_exec_t=call_exec_t, call_dur=call_dur}::other_calls)}]}) = true) then
              true
              else false;

(* Properly set can_sched attribute *)
fun reset_sched_bool thread
 {nid=nid, can_sched=can_sched, node_ths=node_ths} = 

if (will_block thread = true) then {nid=nid, can_sched=true, node_ths=node_ths} else {nid=nid, can_sched=can_sched, node_ths=node_ths};

