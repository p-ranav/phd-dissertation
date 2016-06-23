(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* SCHEDULE GUARD *)
(* IS ANY THREAD IS ALREADY EXECUTING AN OPERATION (ORELSE) IS THE CMQ ON THAT NODE NOT EMPTY = THERE IS SOMETHING TO SCHEDULE *)
(* IMPORTANT RULE: SCHEDULE THREAD ONLY WHEN (NEXT_TICK - CLOCK_VALUE) = CLOCK_TICK I.E. CLOCK IS AT A SCHEDULER TICK *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* Check if all CMQs in a node are empty - Implies that the node has no new operations to service *)
fun IsCMQEmpty [] = true
  | IsCMQEmpty ({component=component, scheme=scheme, queue=queue}::other_threads) =
      if (queue = []) then
          true andalso (IsCMQEmpty other_threads)
      else
          false;

(* Are all threads on Node passive? If so, return true, else return false *)
(* Implies that no operation is currently running (or requires completion) in this node *)
fun ThreadsPassive [] = true
  | ThreadsPassive ({node=node, component=component, priority=priority, operation=operation}::other_threads) =
        if (operation = []) then
            true andalso (ThreadsPassive other_threads)
        else
            false;

(* Check if any thread in current node is schedulable *)
fun CanScheduleOnThisNode clocks {node=node, cmql=cmql} threads rths =
	if (rths = [] andalso threads <> []) then
		if ((ThreadsPassive threads = false) orelse
        (IsCMQEmpty cmql = false)) then
        true
    else
        false
	else
		false;

(* Find any running threads on this node *)
fun FindRunningThreadOnNode this_node [] = []
  | FindRunningThreadOnNode this_node ({node=component_node, component=component, priority=priority, operation=operation}::other_threads) =
  		if (component_node = this_node) then
  			[{node=component_node, component=component, priority=priority, operation=operation}]
  		else (FindRunningThreadOnNode this_node other_threads);

fun CanSchedule clocks ncmql [] rths = false
  | CanSchedule [] ncmql nths rths = false
  | CanSchedule clocks ncmql ({node=node, threads=threads}::other_nodes) rths =
  		(CanScheduleOnThisNode
  							(hd (GetClockOnNode node clocks ))
  						  (hd (GetCMQOnNode node ncmql))
  							threads
  							(FindRunningThreadOnNode node rths))
  		orelse (CanSchedule clocks ncmql other_nodes rths);

(* Schedule Guard *)
fun ScheduleGuard clocks ncmql nths rths =
	if ((LimitReached clocks = false) andalso
     ((TickReached clocks = true)) andalso
	   ((CanSchedule clocks ncmql nths rths) = true)) then
		true
	else
		false;

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* SCHEDULE COMPONENT THREADS - ONE PER NODE BASED *)
(* ONCE THE GUARD IS SATISFIED, PICK THE NEXT COMPONENT THREAD FOR EXECUTION - REMOVE THE APPROPRIATE OPERATION FROM ITS CMQ *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun IsComponentCMQEmpty this_component [] = false
  | IsComponentCMQEmpty this_component ({component=component, scheme=scheme, queue=queue}::other_components) =
      if (component=this_component andalso queue <> []) then
          true
      else
          (IsComponentCMQEmpty this_component other_components);

fun GetNextComponentOperation this_component ({component=component, scheme=scheme, queue=queue}::other_components) =
      if (component=this_component) then
          (hd queue)
      else
          (GetNextComponentOperation this_component other_components);

fun ScheduleThreadOnThisNode {node=cmq_node, cmql=cmql} [] = []
  | ScheduleThreadOnThisNode {node=cmq_node, cmql=cmql}
      ({node=component_node, component=component, priority=priority, operation=operation}::other_threads) =
    if (operation <> []) then
        [{node=component_node, component=component, priority=priority, operation=operation}]
    else
        if (IsComponentCMQEmpty component cmql) then
            [{node=component_node, component=component, priority=priority,
                    operation=operation^^[(GetNextComponentOperation component cmql)]}]
        else
            (ScheduleThreadOnThisNode {node=cmq_node, cmql=cmql} other_threads);

fun ScheduleThreads clocks ncmql [] rths = rths
  | ScheduleThreads clocks ncmql ({node=node, threads=threads}::other_nodes) rths =
    if ((CanScheduleOnThisNode clocks (hd (GetCMQOnNode node ncmql)) threads (FindRunningThreadOnNode node rths)) = true) then
      (ScheduleThreadOnThisNode (hd (GetCMQOnNode node ncmql)) threads)^^(ScheduleThreads clocks ncmql other_nodes rths)
    else
      (ScheduleThreads clocks ncmql other_nodes rths);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* DEQUEUE OPERATION FROM CMQ *)
(* FIND THE LIST OF OPERATIONS SCHEDULED AND REMOVE THEM FROM THE COMPONENT CMQS *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun RemoveOperationFromCMQ this_component this_operation [] = []
  | RemoveOperationFromCMQ this_component this_operation ({component=component, scheme=scheme, queue=queue}::other_threads) =
          if (component = this_component) then
              ({component=component, scheme=scheme, queue=(rm this_operation queue)}::other_threads)
          else
              {component=component, scheme=scheme, queue=queue}::(RemoveOperationFromCMQ this_component this_operation other_threads);

fun UpdateNodeCMQ this_component [] {node=clock_node, value=value, next_tick=next_tick}
                {node=cmq_node, cmql=cmql} = [{node=cmq_node, cmql=cmql}]
  | UpdateNodeCMQ this_component this_operation {node=clock_node, value=value, next_tick=next_tick}
                {node=cmq_node, cmql=cmql} =
          if (this_operation <> []) then
              [{node=cmq_node, cmql = (RemoveOperationFromCMQ this_component (hd this_operation) cmql)}]
          else
              [{node=cmq_node, cmql=cmql}];

fun DequeueOperationOnThisNode this_node clock ncmql nths rths =
      if ((ScheduleThreads [clock] ncmql nths rths) <> []) then
          (UpdateNodeCMQ

              (#component (hd (ScheduleThreads [clock] ncmql nths rths)))
              (#operation (hd (ScheduleThreads [clock] ncmql nths rths)))
              clock
              (hd (GetCMQOnNode this_node ncmql))
          )

      else
          (GetCMQOnNode this_node ncmql);

fun DequeueOperations clocks ncmql [] rths = []
  | DequeueOperations clocks ncmql ({node=node, threads=threads}::other_nodes) rths =
      (DequeueOperationOnThisNode node
                                  (hd(GetClockOnNode node clocks))
                                  ncmql
                                  [{node=node, threads=threads}]
                                  rths)^^
      (DequeueOperations clocks ncmql other_nodes rths);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* REMOVE SCHEDULED THREADS FROM MAIN LIST OF THREADS - IMPLIES THAT THE THREAD IS EXECUTING *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun RemoveThisThread this_node {node=scheduled_node, component=scheduled_component, priority=scheduled_priority, operation=scheduled_operation} [] = []
  | RemoveThisThread this_node {node=scheduled_node, component=scheduled_component, priority=scheduled_priority, operation=scheduled_operation}
                              ({node=node, component=component, priority=priority, operation=operation}::other_threads) =
                if (node=scheduled_node andalso component=scheduled_component) then
                    other_threads
                else
                    {node=node, component=component, priority=priority, operation=operation}
                    ::(RemoveThisThread this_node
                         {node=scheduled_node, component=scheduled_component, priority=scheduled_priority, operation=scheduled_operation} other_threads);

fun RemoveThreadOnThisNode this_node clock ncmql [{node=node, threads=threads}] =
        if ((ScheduleThreads [clock] ncmql [{node=node, threads=threads}] []) <> []) then
            {node=node,
              threads=(RemoveThisThread this_node (hd (ScheduleThreads [clock] ncmql [{node=node, threads=threads}] [])) threads)}
        else
            {node=node, threads=threads};

fun RemoveThreads clocks ncmql [] = []
  | RemoveThreads clocks ncmql ({node=node, threads=threads}::other_nodes) =
          (RemoveThreadOnThisNode node (hd (GetClockOnNode node clocks)) ncmql [{node=node, threads=threads}])
          ::(RemoveThreads clocks ncmql other_nodes);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* THREAD PREEMPTION GUARD *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

(* The running thread needs to be preempted if:
   (1) Clock value = next_tick
   (2) Thread has finished executing an operation i.e. operation = []
 *)
fun CanPreemptThisThread {node=clock_node, value=value, next_tick=next_tick} {node=node, component=component, priority=priority, operation=operation} =
    if (value=next_tick orelse operation = []) then
        true
    else
        false;

fun PreemptGuard clocks [] = false
  | PreemptGuard clocks ({node=node, component=component, priority=priority, operation=operation}::other_threads) =
        (CanPreemptThisThread (hd (GetClockOnNode node clocks)) {node=node, component=component, priority=priority, operation=operation})
        orelse (PreemptGuard clocks other_threads);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* FIND ALL THREADS WAITING TO ENQUEUE ON A GIVEN NODE *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
fun GetWaitingThreadsOnNode this_node [] = []
  | GetWaitingThreadsOnNode this_node ({node=node, unblock_time=unblock_time, thread=thread}::other_threads) =
      if (node=this_node) then
        {node=node, unblock_time=unblock_time, thread=thread}::(GetWaitingThreadsOnNode this_node other_threads)
      else
        (GetWaitingThreadsOnNode this_node other_threads);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* FIND THE NEXT CLOSEST UNBLOCK TIME *)
(* A COMPONENT THREAD NEEDS TO BE ENQUEUED ONTO ITS NODE THREAD LIST AT THIS TIME STAMP AND SO THIS POINT CANNOT BE ACCIDENTALLY SKIPPED DURING "PROGRESS_TIME" *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
fun NextThreadEnqueueTime [] = ~1
  | NextThreadEnqueueTime [{node=node, unblock_time=unblock_time, thread=thread}] = unblock_time
  | NextThreadEnqueueTime ({node=node, unblock_time=unblock_time, thread=thread}::
                                {node=node2, unblock_time=unblock_time2, thread=thread2}::other_threads) =
        if (unblock_time <= unblock_time2) then
          (NextThreadEnqueueTime ({node=node, unblock_time=unblock_time, thread=thread}::other_threads))
        else
          (NextThreadEnqueueTime ({node=node2, unblock_time=unblock_time2, thread=thread2}::other_threads));

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* UNBLOCK A WAITING THREAD WHEN THE RIGHT TIME ARRIVES *)
(* UNBLOCK A THREAD WAITING TO BE UNBLOCKED WHEN ITS NODE CLOCK VALUE REACHES THE UNBLOCK_TIME STAMP *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun CanUnblockThisThread {node=clock_node, value=value, next_tick=next_tick}
                            {node=node, unblock_time=unblock_time, thread=thread} =
            if (value = unblock_time) then
              true
            else
              false

fun UnblockGuard clocks [] = false
  | UnblockGuard clocks ({node=node, unblock_time=unblock_time, thread=thread}::other_waiting_threads) =
      if ((CanUnblockThisThread (hd (GetClockOnNode node clocks)) {node=node, unblock_time=unblock_time, thread=thread}) = true) then
        true
      else
        false orelse (UnblockGuard clocks other_waiting_threads);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* EXECUTE GUARD *)
(* CLOCK LIMIT NOT REACHED + CLOCK != NEXT_TICK + OPERATION NOT COMPLETED *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun CanExecuteThisThread {node=clock_node, value=value, next_tick=next_tick}
                         {node=node, component=component, priority=priority, operation=[]} = false
  | CanExecuteThisThread {node=clock_node, value=value, next_tick=next_tick}
                         {node=node, component=component, priority=priority, operation=operation} =
          if (value < next_tick andalso operation <> []) then
              true
          else
              false;

fun CanExecuteAnyThread clocks [] = false
  | CanExecuteAnyThread clocks ({node=node, component=component, priority=priority, operation=operation}::other_threads) =
      (CanExecuteThisThread
              (hd (GetClockOnNode node clocks))
              {node=node, component=component, priority=priority, operation=operation}
      ) orelse (CanExecuteAnyThread clocks other_threads);

fun ExecuteGuard clocks timers rths wi wths =
  if ((LimitReached clocks = false) andalso
     (EnqueueGuard clocks wi = false) andalso
     (PreemptGuard clocks rths = false) andalso
     (UnblockGuard clocks wths = false) andalso
     (ExpiryGuard clocks timers = false) andalso
     (CanExecuteAnyThread clocks rths = true)) then
    true
  else
    false;

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* EXECUTE THREADS - ONE PER NODE *)
(* NEED TO PROGRESS CLOCK HERE *)
(* OPTIONS:
   (1) PROGRESS CLOCK BY STEP_DUR IF STEP_DUR < (NEXT_TICK - CLOCK_VALUE) AND THERE ARE NO TIMERS OR INTERACTIONS DURING THIS INTERVAL IN THIS NODE
   (2) PROGRESS CLOCK TO NEXT_TICK IF STEP_DUR > (NEXT_TICK - CLOCK_VALUE) AND THERE ARE NO TIMERS OR INTERACTIONS DURING THIS INTERVAL IN THIS NODE
   (3) PROGRESS CLOCK TO NEXT TIMER EXPIRY IF TIMER_EXPIRY_OFFSET < (CLOCK_VALUE + STEP_DUR) AND TIMER_EXPIRY_OFFSET < (NEXT_TICK)
   (4) PROGRESS CLOCK TO ENQUEUE_POINT OF SOME COMPONENT INTERACTION IF ENQUEUE_POINT < (CLOCK_VALUE + STEP_DUR) AND ENQUEUE_POINT < (NEXT_TICK)
 *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

(* Get a list of candidate time stamps and find the minimum - This is the next clock value for 'dynamic time progression' *)
fun FindNextClockValue time_values =
    (FindMinimum (DeleteNumber ~1 time_values));

(* If the operation has no more steps to execute, then the operation is complete - set operation = [] *)
fun UpdateOperationSteps {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
        enqueue_time=enqueue_time, steps=[]}]} =
      {node=thread_node, component=thread_component, priority=thread_priority, operation=[]}
  | UpdateOperationSteps {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
        enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time,
        duration=duration}::other_steps)}]} =
        if (exec_time = duration) then
            if (other_steps = []) then
                {node=thread_node, component=thread_component, priority=thread_priority, operation=[]}
            else
                {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
                enqueue_time=enqueue_time, steps=other_steps}]}
        else
            {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
        enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time,
        duration=duration}::other_steps)}]};

(* Handle local code block - Here, based on the duration of the operation step, compared against the current state of the clock, the amount of time to progress clock by has to be calculated! *)
fun HandleCode {node=clock_node, value=value, next_tick=next_tick} timers waiting_interactions waiting_threads
  {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
        enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
          ::other_steps)}]} =
    if ((duration -  exec_time) > (next_tick - value)) then
      (* Find min time between next_timer_offset, next_operation_enqueue_timestamp, and next_tick and jump to this point so the event can be handled *)
      (* Progress exec_time of the step by this minimum amount *)
      (UpdateOperationSteps
        {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
          enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk,
              exec_time=(exec_time +
                  ((FindNextClockValue ([(NextTimerOffset (GetTimersOnNode node timers))]^^[(NextInteractionEnqueueTime (GetInteractionsOnNode node waiting_interactions))]
                                    ^^[(NextThreadEnqueueTime (GetWaitingThreadsOnNode node waiting_threads))]^^[next_tick]))
                    - value)),
              duration=duration}
          ::other_steps)}]})
    else
      (* Find min time between step_duration, next_timer_offset, next_operation_enqueue_timestamp, and next_tick and jump to this point so the event can be handled *)
      (* Progress exec_time of the step by this minimum amount *)
      (* Since the step will finish before next_tick, update the operation by removing the step if it DOES INFACT finish *)
      (UpdateOperationSteps
        {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
          enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk,
              exec_time=(exec_time +
                  ((FindNextClockValue ([(NextTimerOffset (GetTimersOnNode node timers))]^^[(NextInteractionEnqueueTime (GetInteractionsOnNode node waiting_interactions))]
                                                      ^^[(NextThreadEnqueueTime (GetWaitingThreadsOnNode node waiting_threads))]^^[next_tick]^^[(value + duration - exec_time)]))
                  - value)),
              duration=duration}
          ::other_steps)}]});

(* Handle the interaction by removing the step and then call UpdateOperationSteps before returning the updated token *)
fun HandleInteraction {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
        enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
          ::other_steps)}]} =
    if (kind = "PUBLISHER") then
        (UpdateOperationSteps
          {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
        enqueue_time=enqueue_time, steps=other_steps}]})
    else
      {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
        enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
          ::other_steps)}]};

fun ExecuteOperation clocks timers waiting_interactions waiting_threads
  {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
      enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
        ::other_steps)}]} =
  if (kind = "LOCAL") then
    [(HandleCode
      (hd (GetClockOnNode node clocks)) (* Clock token on operation's node *)
      timers (* List of all timers *)
      waiting_interactions (* List of all operations waiting to enqueue *)
      waiting_threads (* List of all threads waiting to unblock and enqueue onto node threads list *)
      {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
        enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
          ::other_steps)}]})]
  else
    if (kind = "PUBLISHER") then
      [(HandleInteraction
        {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
          enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
            ::other_steps)}]})]
    else
      [];

fun ExecuteThreads clocks timers waiting_interactions waiting_threads [] = []
  | ExecuteThreads clocks timers waiting_interactions waiting_threads ({node=node, component=component, priority=priority, operation=operation}::other_threads) =
      (ExecuteOperation clocks timers waiting_interactions waiting_threads {node=node, component=component, priority=priority, operation=operation})
      ^^(ExecuteThreads clocks timers waiting_interactions waiting_threads other_threads);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* UPDATE THE CLOCK VALUE ON EACH NODE BASED ON THREAD EXECUTION RULES *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
fun UpdateClockOnThisNode {node=clock_node, value=value, next_tick=next_tick} timers waiting_interactions
    {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
      enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
        ::other_steps)}]} =
  if (kind = "LOCAL") then
    if ((duration -  exec_time) > (next_tick - value)) then
      (ProgressClock
          (FindNextClockValue ([(NextTimerOffset (GetTimersOnNode node timers))]^^[(NextInteractionEnqueueTime (GetInteractionsOnNode node waiting_interactions))]^^[next_tick]))
          {node=clock_node, value=value, next_tick=next_tick}
      )
    else
      (* Find min time between step_duration, next_timer_offset, next_operation_enqueue_timestamp, and next_tick and jump to this point so the event can be handled *)
      (* Progress exec_time of the step by this minimum amount *)
      (* Since the step will finish before next_tick, update the operation by removing the step if it DOES INFACT finish *)
      (ProgressClock
          (FindNextClockValue ([(NextTimerOffset (GetTimersOnNode node timers))]^^[(NextInteractionEnqueueTime (GetInteractionsOnNode node waiting_interactions))]
                                                      ^^[next_tick]^^[(value + duration - exec_time)]))
          {node=clock_node, value=value, next_tick=next_tick}
      )
  else
    {node=clock_node, value=value, next_tick=next_tick}

fun UpdateClocks clocks timers waiting_interactions [] = clocks
 | UpdateClocks  clocks timers waiting_interactions ({node=node, component=component, priority=priority, operation=operation}::other_threads) =
      (UpdateClockOnThisNode (hd (GetClockOnNode node clocks)) timers waiting_interactions {node=node, component=component, priority=priority, operation=operation})
      ::(DeleteNumber (hd (GetClockOnNode node clocks)) (UpdateClocks clocks timers waiting_interactions other_threads));

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* UPDATE THE CLOCK VALUES UPON PREEMPTION *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun UpdateClockOnThisNodeOnPreemption {node=clock_node, value=value, next_tick=next_tick} {node=node, component=component, priority=priority, operation=operation} =
  if (value=next_tick orelse operation = []) then
    (GotoNextTick {node=clock_node, value=value, next_tick=next_tick})
  else
    {node=clock_node, value=value, next_tick=next_tick};

fun UpdateClocksOnPreemption clocks [] = clocks
  | UpdateClocksOnPreemption clocks ({node=node, component=component, priority=priority, operation=operation}::other_threads) =
      (UpdateClockOnThisNodeOnPreemption
          (hd (GetClockOnNode node clocks))
          {node=node, component=component, priority=priority, operation=operation}
        )::(UpdateClocksOnPreemption (DeleteNumber (hd (GetClockOnNode node clocks)) clocks) other_threads);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* UPDATE THE RUNNING THREAD LIST UPON PREEMPTION *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun UpdateRunningThreads clocks [] = []
  | UpdateRunningThreads clocks ({node=node, component=component, priority=priority, operation=operation}::other_threads) =
    if (PreemptGuard clocks [{node=node, component=component, priority=priority, operation=operation}] = true) then
      (UpdateRunningThreads clocks other_threads)
    else
      {node=node, component=component, priority=priority, operation=operation}::(UpdateRunningThreads clocks other_threads);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* ENQUEUE THREAD BACK ONTO THREAD LIST UPON PREEMPTION *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
fun EnqueueThread {node=thread_node, component=component, priority=priority, operation=operation} [] =
      [{node=thread_node, component=component, priority=priority, operation=operation}]
  | EnqueueThread {node=thread_node, component=component, priority=priority, operation=operation}
        ({node=thread_node2, component=component2, priority=priority2, operation=operation2}::other_threads) =
        if (priority > priority2) then
          {node=thread_node, component=component, priority=priority, operation=operation}::
          ({node=thread_node2, component=component2, priority=priority2, operation=operation2}::other_threads)
        else
          {node=thread_node2, component=component2, priority=priority2, operation=operation2}::
          (EnqueueThread {node=thread_node2, component=component2, priority=priority2, operation=operation2} other_threads);

fun EnqueueOnThisNode {node=thread_node, component=component, priority=priority, operation=operation} [] = []
  | EnqueueOnThisNode {node=thread_node, component=component, priority=priority, operation=operation}
                   ({node=node, threads=threads}::other_nodes) =
        if (node=thread_node) then
          ({node=node, threads=(EnqueueThread {node=thread_node, component=component, priority=priority, operation=operation} threads)}::other_nodes)
        else
          {node=node, threads=threads}::(EnqueueOnThisNode {node=thread_node, component=component, priority=priority, operation=operation} other_nodes);


fun EstablishOrder clocks [] nths = nths
  | EstablishOrder clocks (first_thread::other_threads) nths =
      if ((PreemptGuard clocks [first_thread]) = true) then
            (EstablishOrder clocks other_threads (EnqueueOnThisNode first_thread nths))
      else
            (EstablishOrder clocks other_threads nths);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* ENQUEUE THREAD BACK ONTO THREAD LIST UPON UNBLOCKING *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun EstablishOrderUponUnblock clocks [] nths = nths
  | EstablishOrderUponUnblock clocks (first_thread::other_threads) nths =
      (EstablishOrderUponUnblock clocks other_threads (EnqueueOnThisNode first_thread nths));

fun RemoveClientStep {node=thread_node, component=thread_component, priority=thread_priority, operation=
                        [{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                            ::other_steps)}]} =
          if (kind = "CLIENT") then
            (UpdateOperationSteps {node=thread_node, component=thread_component, priority=thread_priority, operation=
                        [{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=other_steps}]})
          else
            {node=thread_node, component=thread_component, priority=thread_priority, operation=
                        [{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                            ::other_steps)}]};

fun GetUnblockableThreads clocks [] = []
  | GetUnblockableThreads clocks ({node=node, unblock_time=unblock_time, thread=thread}::other_threads) =
  if ((CanUnblockThisThread (hd (GetClockOnNode node clocks)) {node=node, unblock_time=unblock_time, thread=thread}) = true) then
      [(RemoveClientStep thread)]^^(GetUnblockableThreads clocks other_threads)
  else
      (GetUnblockableThreads clocks other_threads);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* REMOVE THREADS FROM WAITING THREAD LIST UPON UNBLOCKING *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun GetThreadsToRemoveFromWTHS clocks [] = []
  | GetThreadsToRemoveFromWTHS clocks ({node=node, unblock_time=unblock_time, thread=thread}::other_threads) =
  if ((CanUnblockThisThread (hd (GetClockOnNode node clocks)) {node=node, unblock_time=unblock_time, thread=thread}) = true) then
      [thread]^^(GetThreadsToRemoveFromWTHS clocks other_threads)
  else
      (GetThreadsToRemoveFromWTHS clocks other_threads);

fun RemoveThisUnblockedThread this_thread [] = []
  | RemoveThisUnblockedThread this_thread ({node=node, unblock_time=unblock_time, thread=thread}::other_threads) =
      if (this_thread = thread) then
        other_threads
      else
        {node=node, unblock_time=unblock_time, thread=thread}::(RemoveThisUnblockedThread this_thread other_threads);

fun RemoveUnblockedThreads [] wths = wths
  | RemoveUnblockedThreads (first_thread::other_threads) wths =
        (RemoveUnblockedThreads other_threads (RemoveThisUnblockedThread first_thread wths));

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* INDUCE OPERATIONS - PLACE INDUCED OPERATIONS INTO A WAITING LIST FOR ENQUEUE IN THE (POTENTIALLY IMMEDIATE) FUTURE *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun GetAllInducedOperations {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                            ::other_steps)}]} (* One of the currently running threads *)

                            [] {node=clock_node, value=value, next_tick=next_tick} = []

  | GetAllInducedOperations {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                            ::other_steps)}]} (* One of the currently running threads *)

                           ({node=interaction_node, port=interaction_port, operation={node=operation_node, component=operation_component, operation=operation, priority=operation_priority,
                              deadline=operation_deadline, enqueue_time=operation_enqueue_time, steps=operation_steps}}::other_interactions)
                           (* List of potential interactions - that can be induced on some CMQ *)

                           {node=clock_node, value=value, next_tick=next_tick} (* Clock state of the running thread *) =

            if (thread_node=interaction_node andalso port=interaction_port andalso (List.exists (fn z => z = kind) ["CLIENT", "PUBLISHER"])) then
                {node=operation_node, enqueue_time=value, operation={node=operation_node, component=operation_component, operation=operation, priority=operation_priority,
                  deadline=operation_deadline, enqueue_time=value, steps=operation_steps}}::
                  (GetAllInducedOperations
                      {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                            ::other_steps)}]}

                      other_interactions
                      {node=clock_node, value=value, next_tick=next_tick}
                    )
            else
                (GetAllInducedOperations
                      {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                            ::other_steps)}]}

                      other_interactions
                      {node=clock_node, value=value, next_tick=next_tick});

fun InduceOperations wi rths [] clocks = []
  | InduceOperations wi [] interactions clocks = []
  | InduceOperations wi ({node=thread_node, component=component, priority=priority, operation=operation}::other_threads)
                        ({node=interaction_node, port=interaction_port, operation=induced_operation}::other_interactions)
                        clocks =
        wi^^((GetAllInducedOperations {node=thread_node, component=component, priority=priority, operation=operation} (* Currently Running Thread *)
                                     ({node=interaction_node, port=interaction_port, operation=induced_operation}::other_interactions) (* Potential interactions *)
                                     (hd (GetClockOnNode thread_node clocks)))
        ^^(InduceOperations wi other_threads ({node=interaction_node, port=interaction_port, operation=induced_operation}::other_interactions) clocks));


(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* ADVANCE TIME - THIS IS A LOW PRIORITY TRANSITION THAT WILL FIRE WHEN NOTHING ELSE CAN HAPPEN *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun AdvanceTimeGuard clocks wths wi =
  if ((LimitReached clocks = false))  then
    true
  else
    false;

fun FindMinimumAdvanceTime [] timers waiting_interactions waiting_threads ncmql nths rths = ~1
  | FindMinimumAdvanceTime ({node=node, value=value, next_tick=next_tick}::other_clocks) timers waiting_interactions waiting_threads ncmql nths rths =
    (FindMinimum
        (DeleteNumber ~1 ([(NextTimerOffset (GetTimersOnNode node timers))]^^[(NextInteractionEnqueueTime (GetInteractionsOnNode node waiting_interactions))]
                            ^^[(NextThreadEnqueueTime (GetWaitingThreadsOnNode node waiting_threads))]
                            ^^[(FindMinimumAdvanceTime other_clocks timers waiting_interactions waiting_threads ncmql nths rths)])
        )
      );

fun FindMinimumAdvanceTimeWithNextTick [] timers waiting_interactions waiting_threads ncmql nths rths = ~1
  | FindMinimumAdvanceTimeWithNextTick ({node=node, value=value, next_tick=next_tick}::other_clocks) timers waiting_interactions waiting_threads ncmql nths rths =
    (FindMinimum
        (DeleteNumber ~1 ([(NextTimerOffset (GetTimersOnNode node timers))]^^[(NextInteractionEnqueueTime (GetInteractionsOnNode node waiting_interactions))]
                            ^^[(NextThreadEnqueueTime (GetWaitingThreadsOnNode node waiting_threads))]
                            ^^[next_tick]
                            ^^[(FindMinimumAdvanceTimeWithNextTick other_clocks timers waiting_interactions waiting_threads ncmql nths rths)])
        )
      );

fun GetAdvanceTime clocks timers waiting_interactions waiting_threads ncmql nths rths =
  (* IF NO THREAD CAN BE SCHEDULED IN THE NEXT TICK THEN - Jump to next timer or next enqueue point whichever is closest (across all nodes) *)
  if ((ScheduleGuard (GotoNextTickAllClocks clocks) ncmql nths rths) = false) then
    (FindMinimumAdvanceTime clocks timers waiting_interactions waiting_threads ncmql nths rths)
  else
    (FindMinimumAdvanceTimeWithNextTick clocks timers waiting_interactions waiting_threads ncmql nths rths);

fun AdvanceTime by_this_much [] timers waiting_interactions waiting_threads = []
  | AdvanceTime by_this_much (first_clock::other_clocks) timers waiting_interactions waiting_threads =
        (ProgressClock by_this_much first_clock)
        ::(AdvanceTime by_this_much other_clocks timers waiting_interactions waiting_threads);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* SAVE COMPLETED OPERATIONS *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun SaveOperation clocks timers waiting_interactions waiting_threads
  {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
      enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
        ::other_steps)}]} =
  if (kind = "PUBLISHER" andalso other_steps = []) then
    [{node=thread_node, component=thread_component, operation=operation, enqueue_time=enqueue_time, completion_time=(GetClockValue (hd (GetClockOnNode thread_node clocks))), deadline=deadline}]
  else
    if (kind = "LOCAL" andalso other_steps = []) then
      if ((ExecuteOperation clocks timers waiting_interactions waiting_threads
              {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
                enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
              ::other_steps)}]})
          = [{node=thread_node, component=thread_component, priority=thread_priority, operation=[]}]) then
        [{node=thread_node, component=thread_component, operation=operation, enqueue_time=enqueue_time,
          completion_time = (GetClockValue (hd (GetClockOnNode thread_node (UpdateClocks clocks timers waiting_interactions
                    [{node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
                      enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                      ::other_steps)}]}] )  )) ), deadline=deadline}]
      else
        []
    else
      [];

fun GetCompletedOperations clocks timers waiting_interactions waiting_threads [] = []
  | GetCompletedOperations clocks timers waiting_interactions waiting_threads ({node=node, component=component, priority=priority, operation=operation}::other_threads) =
      (SaveOperation clocks timers waiting_interactions waiting_threads {node=node, component=component, priority=priority, operation=operation})
      ^^(GetCompletedOperations clocks timers waiting_interactions waiting_threads other_threads);

fun SaveAllCompletedOperations copns clocks timers waiting_interactions waiting_threads rths =
    copns^^(GetCompletedOperations clocks timers waiting_interactions waiting_threads rths);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* BLOCK RUNNING THREAD UPON COMPLETION OF A BLOCKING REMOTE METHOD INVOCATION *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun BlockThisThread clocks timers waiting_interactions waiting_threads
  {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
      enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
        ::other_steps)}]} =
  if (kind = "CLIENT") then
    if ((ExecuteOperation clocks timers waiting_interactions waiting_threads
              {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
                enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
              ::other_steps)}]}) = []) then
      [{node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
                enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                ::other_steps)}]}]
    else
      []
  else
    [];

fun GetBlockedThreads clocks timers waiting_interactions waiting_threads [] = []
  | GetBlockedThreads clocks timers waiting_interactions waiting_threads ({node=node, component=component, priority=priority, operation=operation}::other_threads) =
      (BlockThisThread clocks timers waiting_interactions waiting_threads {node=node, component=component, priority=priority, operation=operation})
      ^^(GetBlockedThreads clocks timers waiting_interactions waiting_threads other_threads);

fun GetAllBlockedThreads bths clocks timers waiting_interactions waiting_threads rths =
  bths^^(GetBlockedThreads clocks timers waiting_interactions waiting_threads rths);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* PLACE UNBLOCKED THREADS IN A WAITING LIST - THESE THREADS WILL ACTUALLY BE UNBLOCKED WHEN THE CLOCK VALUE ON THAT NODE REACHES THE APPROPRIATE TIME STAMP *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun GetUnblockedThreads {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                            ::other_steps)}]} (* One of the currently running threads *)
                        []
                        {node=clock_node, value=value, next_tick=next_tick} all_clocks
                        timers wi wths = []

  | GetUnblockedThreads {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                            ::other_steps)}]} (* One of the currently running threads *)

                        ({node=blocked_node, component=blocked_component, priority=blocked_priority,
                          operation=
                              [{node=operation_node, component=operation_component, operation=operation, priority=operation_priority,
                              deadline=operation_deadline, enqueue_time=operation_enqueue_time,
                              steps=
                                  ({kind=operation_kind, port=operation_port, unblk=operation_unblk, exec_time=operation_exec_time, duration=operation_duration}
                                    ::operation_other_steps)}]
                          }::other_blocked_threads)

                        {node=clock_node, value=value, next_tick=next_tick} all_clocks
                        timers wi wths =

            if ((operation_kind = "CLIENT")

                andalso
                  (* CURRENTLY RUNNING OPERATION IS ABOUT TO COMPLETE - AFTER THIS EXECUTION *)
                  (ExecuteOperation [{node=clock_node, value=value, next_tick=next_tick}] timers wi wths
                        {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                            ::other_steps)}]})
                      = [{node=thread_node, component=thread_component, priority=thread_priority, operation=[]}]

                  andalso
                  (* IF BLOCKED THREAD EXISTS IN THE UNBLK LIST OF THE CURRENTLY RUNNING THREAD'S OPERATIONAL STEP *)
                  (contains unblk [{node=blocked_node, component=blocked_component, port=operation_port}]))
                  then
                      [{node=blocked_node, unblock_time=(GetClockValue
                                                          (hd (GetClockOnNode thread_node
                                                            (UpdateClocks all_clocks timers wi
                                                              [{node=thread_node, component=thread_component, priority=thread_priority,
                                                              operation=[{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time,
                                                              steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                                                            ::other_steps)}]}])
                                                          ))
                                                        ),
                        thread={node=blocked_node, component=blocked_component, priority=blocked_priority,
                          operation=
                              [{node=operation_node, component=operation_component, operation=operation, priority=operation_priority,
                              deadline=operation_deadline, enqueue_time=operation_enqueue_time,
                              steps=({kind=operation_kind, port=operation_port, unblk=operation_unblk, exec_time=operation_exec_time, duration=operation_duration}
                                    ::operation_other_steps)}]}}]
            else
              (GetUnblockedThreads
                {node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=thread_operation, priority=priority, deadline=deadline,
                            enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
                            ::other_steps)}]}
                other_blocked_threads
                {node=clock_node, value=value, next_tick=next_tick} all_clocks timers wi wths);

fun UnblockThreads wths rths [] clocks timers wi = []
  | UnblockThreads wths [] bths clocks timers wi = []
  | UnblockThreads wths ({node=thread_node, component=component, priority=priority, operation=operation}::other_threads)
                        ({node=blocked_node, component=blocked_component, priority=blocked_priority, operation=blocked_operation}::other_blocked_threads)
                        clocks timers wi =
          wths^^((GetUnblockedThreads {node=thread_node, component=component, priority=priority, operation=operation} (* Currently Running Thread *)
                                      ({node=blocked_node, component=blocked_component, priority=blocked_priority, operation=blocked_operation}::other_blocked_threads) (* Potential unblocks *)
                                      (hd (GetClockOnNode thread_node clocks))
                                      clocks
                                      timers wi wths)
          ^^(UnblockThreads wths other_threads
                ({node=blocked_node, component=blocked_component, priority=blocked_priority, operation=blocked_operation}::other_blocked_threads)
                        clocks timers wi));

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* REMOVE UNBLOCKED THREADS FROM BLOCKED_THREADS LIST *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
fun RemoveBlockedThreads [] blocked_threads = blocked_threads
  | RemoveBlockedThreads ({node=node, unblock_time=unblock_time, thread=thread}::other_unblocked_threads) blocked_threads =
      (RemoveBlockedThreads other_unblocked_threads (DeleteNumber thread blocked_threads))

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* SAVE UNBLOCKED AND COMPLETED OPERATIONS *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
fun GetUnblockedCompletedOperation {node=blocked_node, unblock_time=unblock_time,
      thread={node=thread_node, component=thread_component, priority=thread_priority, operation=[{node=node, component=component, operation=operation, priority=priority, deadline=deadline,
      enqueue_time=enqueue_time, steps=({kind=kind, port=port, unblk=unblk, exec_time=exec_time, duration=duration}
        ::other_steps)}]}} =
      if (other_steps = []) then
          [{node=thread_node, component=thread_component, operation=operation, enqueue_time=enqueue_time, completion_time=unblock_time, deadline=deadline}]
      else
        [];

fun SaveUnblockedCompletedOperations copns [] = []
  | SaveUnblockedCompletedOperations copns ({node=node, unblock_time=unblock_time, thread=thread}::other_wths) =
    copns^^((GetUnblockedCompletedOperation {node=node, unblock_time=unblock_time, thread=thread})
            ^^(SaveUnblockedCompletedOperations copns other_wths));

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* INITIALIZE COMPETING THREADS *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
fun Initialize this_thread nths =
      (EnqueueOnThisNode this_thread nths);
