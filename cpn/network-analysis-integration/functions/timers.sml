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
                                       priority=priority, deadline=deadline, enqueue_time=enqueue_time, data=data,steps=steps}}::other_timers) = 
            if (node = timer_node andalso value = offset) then 
            ({node=opn_node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=value, data=data, steps=steps}
            ::(GetTimerOperations {node=node, value=value, next_tick=next_tick} other_timers))
            else 
            (GetTimerOperations {node=node, value=value, next_tick=next_tick} other_timers); 

(* Find operations to prepare for enqueue *)
fun ListTimerOperations [] timers = []
 | ListTimerOperations (clock::other_clocks) timers = 
      (GetTimerOperations clock timers)^^(ListTimerOperations other_clocks timers); 

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* UPDATE OPERATION NETWORK DATA FOR PERIODICITY - HARD CODED FOR CONVENIENCE - WARNING: NEEDS TO BE UPDATED *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
fun comparator x y = 
  if (Int.compare(x,y) = EQUAL) then 
     1
  else 
      0;

fun DataFunction current_data clock_value amplitude =

(6000 + (Real.floor (6000.0 * amplitude * (Math.sin ( (Real.fromInt (clock_value mod 9000)) * 2.0 * 3.14 / (Real.fromInt 9000)) ))));

fun UpdateNetworkData clocks {node=opn_node, component=opn_component, operation=opn_timer, priority=opn_priority, deadline=opn_deadline, enqueue_time=opn_enq_time,
                                        data=opn_data, steps=[{kind="LOCAL", port="LOCAL", unblk=[], exec_time=exec_time_0, duration=duration_0, max_data_size=max_data_size_0},
                                               {kind="PUBLISHER", port=port_name, unblk=[], exec_time=exec_time_1, duration=duration_1, max_data_size=max_data_size_1}, 
                                               {kind="LOCAL", port="LOCAL", unblk=[], exec_time=exec_time_2, duration=duration_2, max_data_size=max_data_size_2}]} =

      {node=opn_node, component=opn_component, operation=opn_timer, priority=opn_priority, deadline=opn_deadline, enqueue_time=opn_enq_time,
       data=(DataFunction max_data_size_1 (GetClockValue (hd (GetClockOnNode opn_node clocks))) 0.3), steps=[{kind="LOCAL", port="LOCAL", unblk=[], exec_time=exec_time_0, duration=duration_0, max_data_size=max_data_size_0},
              {kind="PUBLISHER", port=port_name, unblk=[], exec_time=exec_time_1, duration=duration_1, max_data_size=(DataFunction max_data_size_1 (GetClockValue (hd (GetClockOnNode opn_node clocks))) 0.3)}, 
              {kind="LOCAL", port="LOCAL", unblk=[], exec_time=exec_time_2, duration=duration_2, max_data_size=max_data_size_2}]}

  | UpdateNetworkData  clocks {node=opn_node, component=opn_component, operation=opn_timer, priority=opn_priority, deadline=opn_deadline, enqueue_time=opn_enq_time, data=opn_data, steps=steps} =
    {node=opn_node, component=opn_component, operation=opn_timer, priority=opn_priority, deadline=opn_deadline, enqueue_time=opn_enq_time, data=opn_data, steps=steps};

fun UpdateNetworkData2 {node=clock_node, value=value, next_tick=next_tick}
                       {node=opn_node, component=opn_component, operation=opn_timer, priority=opn_priority, deadline=opn_deadline, enqueue_time=opn_enq_time,
                                        data=opn_data, steps=[{kind="LOCAL", port="LOCAL", unblk=[], exec_time=exec_time_0, duration=duration_0, max_data_size=max_data_size_0},
                                               {kind="PUBLISHER", port=port_name, unblk=[], exec_time=exec_time_1, duration=duration_1, max_data_size=max_data_size_1}, 
                                               {kind="LOCAL", port="LOCAL", unblk=[], exec_time=exec_time_2, duration=duration_2, max_data_size=max_data_size_2}]} =

      {node=opn_node, component=opn_component, operation=opn_timer, priority=opn_priority, deadline=opn_deadline, enqueue_time=value,
       data=opn_data, steps=[{kind="LOCAL", port="LOCAL", unblk=[], exec_time=exec_time_0, duration=duration_0, max_data_size=max_data_size_0},
              {kind="PUBLISHER", port=port_name, unblk=[], exec_time=exec_time_1, duration=duration_1, max_data_size=(DataFunction max_data_size_1 value 0.3)}, 
              {kind="LOCAL", port="LOCAL", unblk=[], exec_time=exec_time_2, duration=duration_2, max_data_size=max_data_size_2}]}

  | UpdateNetworkData2  {node=clock_node, value=value, next_tick=next_tick}
           {node=opn_node, component=opn_component, operation=opn_timer, priority=opn_priority, deadline=opn_deadline, enqueue_time=opn_enq_time, data=opn_data, steps=steps} =
    {node=opn_node, component=opn_component, operation=opn_timer, priority=opn_priority, deadline=opn_deadline, enqueue_time=opn_enq_time, data=(DataFunction 0 value 0.3), steps=steps}; 

fun UpdateDataSize {node=clock_node, value=value, next_tick=next_tick}
                       {node=opn_node, component=opn_component, operation=opn_timer, priority=opn_priority, deadline=opn_deadline, enqueue_time=opn_enq_time,
                                        data=opn_data, steps=[{kind="LOCAL", port="LOCAL", unblk=[], exec_time=exec_time_0, duration=duration_0, max_data_size=max_data_size_0},
                                               {kind="PUBLISHER", port=port_name, unblk=[], exec_time=exec_time_1, duration=duration_1, max_data_size=max_data_size_1}, 
                                               {kind="LOCAL", port="LOCAL", unblk=[], exec_time=exec_time_2, duration=duration_2, max_data_size=max_data_size_2}]} =

      (DataFunction max_data_size_1 value 0.3)

  | UpdateDataSize  {node=clock_node, value=value, next_tick=next_tick}
           {node=opn_node, component=opn_component, operation=opn_timer, priority=opn_priority, deadline=opn_deadline, enqueue_time=opn_enq_time, 
           data=opn_data, steps=steps} =
       (DataFunction 0 value 0.3);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* UPDATE TIMER OFFSETS AFTER TIMER EXPIRY IS HANDLED *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

(* Update Timer Offsets *)
fun UpdateTimers clocks [] = []
  | UpdateTimers clocks ({node=timer_node, period=period, offset=offset, operation=operation}::other_timers) = 
         if (((ExpiryGuard clocks [{node=timer_node, period=period, offset=offset, operation=operation}]) = true) andalso (period > 0)) then
         	({node=timer_node, period=period, offset=offset+period, operation=(UpdateNetworkData clocks operation)}::(UpdateTimers clocks other_timers))
         else
         	({node=timer_node, period=period, offset=offset-1, operation=operation}::(UpdateTimers clocks other_timers));

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