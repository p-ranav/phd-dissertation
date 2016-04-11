(* Guard for Timer Expiry *)
fun timer_check {t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op}
				{clock_node=clock_node, clock_value=clock_value, schedule=schedule} = 
			(* Node check + offset check *)
			if (t_nid = clock_node andalso t_off = clock_value andalso clock_value < clock_limit) then true else false;	

(* Upate timer offset after expiry *)
fun update_timer {t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op} = 
			if (t_pr <> 0) then 
				 1`{t_nid=t_nid, t_pr=t_pr, t_off=t_off+t_pr, t_op=t_op}		   	
			else empty;	 

(* Get timer operation *)
fun get_t_op {t_nid=t_nid, t_pr=t_pr, t_off=t_off, t_op=t_op} = t_op;