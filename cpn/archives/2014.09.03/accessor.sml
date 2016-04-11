(* Given a node, give me the node clock from the node clock list *)
(* CAUTION - This returns a list - Take the head *)
fun get_node_clock node_name [] = []
  | get_node_clock node_name 
		({clock_node=clock_node, clock_value=clock_value, schedule=schedule}::other_node_clocks) = 

		if (clock_node = node_name) then 
			[{clock_node=clock_node, clock_value=clock_value, schedule=schedule}]
		else 
			(get_node_clock node_name other_node_clocks);

(* Given a node, give me the node cmq from the node cmq list *)
(* CAUTION - This returns a list - Take the head *)
fun get_node_cmq node_name [] = []
  | get_node_cmq node_name
  		({cmq_node=cmq_node, cmqs=cmqs}::other_cmqs) = 
  	if (cmq_node = node_name) then [{cmq_node=cmq_node, cmqs=cmqs}]
  	else (get_node_cmq node_name other_cmqs);

(* Given a node, give me the timers from that node *)
fun get_timers node_name [] = []
  | get_timers node_name
  			   ({t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}::other_timers) = 
  	if (t_nid = node_name) then 
  		{t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}::(get_timers node_name other_timers)
  	else 
  		(get_timers node_name other_timers);


(* Have all clocks reached the clock limit? *)
fun reached_limit [] = true
  | reached_limit ({clock_node=clock_node, clock_value=clock_value, schedule=schedule}::other_node_clocks) = 
    if (clock_value >= clock_limit) then 
        true andalso (reached_limit other_node_clocks)
    else false;

