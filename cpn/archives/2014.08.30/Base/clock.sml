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
fun progress_schedule x {clock_node=clock_node, clock_value=clock_value, schedule=[]} = 
			{clock_node=clock_node, clock_value=clock_value, schedule=[]}
  | progress_schedule x {clock_node=clock_node, clock_value=clock_value, 
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