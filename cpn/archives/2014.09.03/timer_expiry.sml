(* Get expired timers in node *)
fun get_expired_timer_ops {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
						[] = []
  | get_expired_timer_ops {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
                       ({t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}::other_timers) = 
            if (clock_node = t_nid andalso clock_value = t_off) then 
            	   [t_op]^^(get_expired_timer_ops {clock_node=clock_node, clock_value=clock_value, schedule=schedule} other_timers)
           	else 
           		(get_expired_timer_ops {clock_node=clock_node, clock_value=clock_value, schedule=schedule} other_timers);	
(* Main Timer expiry function *)
fun expire [] timers = []
  | expire (first_node_clock::other_clocks)
                timers = 
        (get_expired_timer_ops first_node_clock timers)^^(expire other_clocks timers);


(* Check if any timers have expired in node *)
fun expiry_check {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
				  [] = false
  | expiry_check {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
                       ({t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}::other_timers) = 
            if (clock_node = t_nid andalso clock_value = t_off) then 
            		true
            else (expiry_check {clock_node=clock_node, clock_value=clock_value, schedule=schedule}
                       other_timers);

(* Timer Expiry Guard *)
fun timer_guard [] timers = false
  | timer_guard (first_node_clock::other_clocks)
				 timers = 

  if (reached_limit (first_node_clock::other_clocks) = false) then
  		if ((expiry_check first_node_clock timers) = true) then true
  		else 
  			(timer_guard other_clocks timers)
  else 
      false;


(* Update Timer Offsets *)
fun update_timers clocks [] = []
  | update_timers clocks ({t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}::other_timers) = 
		if ((timer_guard clocks [{t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}]) = true) then 
			({t_nid=t_nid, t_pr=t_pr, t_off=t_off+t_pr, t_op=t_op}::(update_timers clocks other_timers))
		else 
			({t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}::(update_timers clocks other_timers));

