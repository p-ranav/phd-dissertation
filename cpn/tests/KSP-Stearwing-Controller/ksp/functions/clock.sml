(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* CONSTANTS *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  

(* Time Resolution of Analysis *)
val time_res = 100;

(* Clock Tick *)
val clock_tick = 1000;

(* Clock Limit - Upper bound on clock value to bound state space generation *)
val clock_limit = 500000000;

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* GETTERS *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  

(* Get Current Clock Value *)
fun GetClockValue {node=node, value=value, next_tick=next_tick} = value;

(* Get Next tick time stamp *)
fun GetNextTick {node=node, value=value, next_tick=next_tick} = next_tick;

(* Get Clock state for a given node *)
fun GetClockOnNode this_node [] = []
  | GetClockOnNode this_node ({node=node, value=value, next_tick=next_tick}::other_nodes) = 
      if (node = this_node) then
        [{node=node, value=value, next_tick=next_tick}]
      else (GetClockOnNode this_node other_nodes);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* CHECK IF CLOCK LIMIT IS REACHED *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)        	

(* Check if a node clock has reached its clock limit *)
fun LimitReached [] = true
 |  LimitReached ({node=node, value=value, next_tick=next_tick}::other_nodes) = 
      if (value = clock_limit) then  
         true andalso (LimitReached other_nodes)
      else false; 

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* CHECK IF CLOCK TICK IS REACHED *)
(* SCHEDULER WILL SCHEDULE THREADS ONLY AT CLOCK TICKS *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)       
fun TickReached [] = false
  | TickReached ({node=node, value=value, next_tick=next_tick}::other_nodes) = 
  	  if (next_tick - value <= clock_tick) then 
  	  	true
  	  else 
  	  	false orelse (TickReached other_nodes);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* PROGRESS CLOCK - PROGRESSES CLOCK VALUE TO SOME TIME STAMP AND UPDATES NEXT_TICK*)
(* GOTO NEXT TICK - PROGRESSES CLOCK VALUE TO NEXT_TICK AND UPDATES NEXT_TICK *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
fun GotoNextTick {node=node, value=value, next_tick=next_tick} = 
      {node=node, value=next_tick, next_tick=next_tick + clock_tick};

fun GotoNextTickAllClocks [] = []
  | GotoNextTickAllClocks ({node=node, value=value, next_tick=next_tick}::other_clocks) = 
      (GotoNextTick {node=node, value=value, next_tick=next_tick})::(GotoNextTickAllClocks other_clocks);


fun ProgressClock this_value {node=node, value=value, next_tick=next_tick} = 
   if (next_tick < this_value) then 
        (ProgressClock this_value (GotoNextTick {node=node, value=value, next_tick = next_tick}))  
   else
        {node=node, value=this_value, next_tick=next_tick};      
