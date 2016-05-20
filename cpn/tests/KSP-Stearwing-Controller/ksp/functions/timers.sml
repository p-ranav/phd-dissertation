(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* TIMER GUARD *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  

(* Check for timer expiry *)
fun TimerExpired {node=node, value=value, next_tick=next_tick} [] = false
  | TimerExpired {node=node, value=value, next_tick=next_tick} 
                 ({node=timer_node, period=period, offset=offset, operation=operation}::other_timers) = 
         if (node = timer_node andalso value = offset) then 
			true
		 else (TimerExpired {node=node, value=value, next_tick=next_tick} other_timers);   


(* Guard for Timer_Expiry Transition *)  	
fun ExpiryGuard [] [] = false
  | ExpiryGuard [] timers = false
  | ExpiryGuard (clock::other_clocks) [] = false
  | ExpiryGuard (clock::other_clocks) timers = 
       if (LimitReached (clock::other_clocks) = false) then 
     	  if (TimerExpired clock timers = true) then
     	     true
     	  else 
     	  	 (ExpiryGuard other_clocks timers)
       else
          false; 

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* RETURN LIST OF TIMER OPERATIONS *)
(* SET ENQUEUE_TIME OF OPERATION TO CLOCK VALUE *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)            

(* Get Timer Operations *)
fun GetTimerOperations {node=node, value=value, next_tick=next_tick} [] = []
  | GetTimerOperations {node=node, value=value, next_tick=next_tick}
                       ({node=timer_node, period=period, offset=offset, 
                       operation={node=opn_node, component=opn_component, operation=operation, 
                                       priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}}::other_timers) = 
            if (node = timer_node andalso value = offset) then 
            ({node=opn_node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=value, steps=steps}
            ::(GetTimerOperations {node=node, value=value, next_tick=next_tick} other_timers))
            else 
            (GetTimerOperations {node=node, value=value, next_tick=next_tick} other_timers); 

(* Find operations to prepare for enqueue *)
fun ListTimerOperations [] timers = []
 | ListTimerOperations (clock::other_clocks) timers = 
      (GetTimerOperations clock timers)^^(ListTimerOperations other_clocks timers); 

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* UPDATE TIMER OFFSETS AFTER TIMER EXPIRY IS HANDLED *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)         

(* Update Timer Offsets *)
fun UpdateTimers clocks [] = []
  | UpdateTimers clocks ({node=timer_node, period=period, offset=offset, operation=operation}::other_timers) = 
         if ((ExpiryGuard clocks [{node=timer_node, period=period, offset=offset, operation=operation}]) = true) then
         	({node=timer_node, period=period, offset=offset+period, operation=operation}::(UpdateTimers clocks other_timers))
         else
         	({node=timer_node, period=period, offset=offset, operation=operation}::(UpdateTimers clocks other_timers));

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* GET ALL TIMERS ON A GIVEN NODE *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)           
fun GetTimersOnNode this_node [] = []
  | GetTimersOnNode this_node ({node=node, period=period, offset=offset, operation=operation}::other_timers) = 
      if (node=this_node) then 
        {node=node, period=period, offset=offset, operation=operation}::(GetTimersOnNode this_node other_timers)
      else
        (GetTimersOnNode this_node other_timers); 

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* FIND NEXT TIMER OFFSET - TIME STAMP WHEN NEXT CLOSEST TIMER WILL EXPIRE *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)   
fun NextTimerOffset [] = ~1
  | NextTimerOffset [{node=node, period=period, offset=offset, operation=operation}] = offset
  | NextTimerOffset ({node=node, period=period, offset=offset, operation=operation}::
                     {node=node2, period=period2, offset=offset2, operation=operation2}::other_timers) = 
      if (offset <= offset2) then 
        (NextTimerOffset ({node=node, period=period, offset=offset, operation=operation}::other_timers))
      else
        (NextTimerOffset ({node=node2, period=period2, offset=offset2, operation=operation2}::other_timers));
