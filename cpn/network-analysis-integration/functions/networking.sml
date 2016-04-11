(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* Network Delay Calculation - Helper Functions *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)        	
fun GetTimeAtData data [{timestamp=prev_timestamp, data=prev_data, bandwidth=prev_bandwidth}] [] = [prev_timestamp]
  | GetTimeAtData data [] [] = [] 
  | GetTimeAtData data [{timestamp=prev_timestamp, data=prev_data, bandwidth=prev_bandwidth}]
		   ({timestamp=resource_timestamp, data=resource_data, bandwidth=resource_bandwidth}::other_units) =
  if(prev_data <= data andalso resource_data >= data) then
      [resource_timestamp + ( (data - prev_data) div resource_bandwidth)]
  else
      if (prev_data > data) then
	  [resource_timestamp - ((prev_data - data) div prev_bandwidth)]
      else
	  (GetTimeAtData data [{timestamp=resource_timestamp, data=resource_data, bandwidth=resource_bandwidth}] other_units);

fun GetDataAtTime time [{timestamp=prev_timestamp, data=prev_data, bandwidth=prev_bandwidth}] [] = [prev_data]
  | GetDataAtTime time [] [] = []
  | GetDataAtTime time [{timestamp=prev_timestamp, data=prev_data, bandwidth=prev_bandwidth}]
		  ({timestamp=resource_timestamp, data=resource_data, bandwidth=resource_bandwidth}::other_units) =
  if (prev_timestamp <= time andalso resource_timestamp >= time) then
      [(prev_data - (prev_bandwidth * (resource_timestamp - time))  )]
  else
      (GetDataAtTime time [{timestamp=resource_timestamp, data=resource_data, bandwidth=resource_bandwidth}] other_units);

fun GetLastResourceData ({timestamp=resource_timestamp, data=resource_data, bandwidth=resource_bandwidth}::other_units) =
  if (other_units = []) then
      resource_data
  else
      (GetLastResourceData other_units);

fun GetFirstResource [] = []
  | GetFirstResource ({timestamp=resource_timestamp, data=resource_data, bandwidth=resource_bandwidth}::other_units) =
  [{timestamp=resource_timestamp, data=resource_data, bandwidth=resource_bandwidth}];

fun GetOffsetData start period ({timestamp=resource_timestamp, data=resource_data, bandwidth=resource_bandwidth}::other_units) =
  (GetDataAtTime (start mod period)
		 [{timestamp=resource_timestamp, data=resource_data, bandwidth=resource_bandwidth}]
		 other_units);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* Network Delay Calculation Function *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  
fun GetDelay data start period resources =
  if ((data mod (GetLastResourceData resources) ) > ((GetLastResourceData resources) - (hd (GetOffsetData start period resources)) )) then 

      ((data div (GetLastResourceData resources)) * period + (period - (start mod period)) +
      (hd (GetTimeAtData ((data mod (GetLastResourceData resources)) - ((GetLastResourceData resources) - (hd (GetOffsetData start period resources))))
     (GetFirstResource resources)
     (tl resources))))
  else
      ((data div (GetLastResourceData resources)) * period +
      (hd (GetTimeAtData ((data mod (GetLastResourceData resources)) + (hd (GetOffsetData start period resources)))
	  (GetFirstResource resources)
	  (tl resources))) - (start mod period));
      
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* Network Dequeue Function *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
fun GetNodeProfile this_node ({node=node, period=period, profile=profile}::other_nodes) =
  if (this_node = node) then
      profile
  else
      (GetNodeProfile this_node other_nodes);

fun GetNodePeriod this_node ({node=node, period=period, profile=profile}::other_nodes) =
  if (this_node = node) then
      period
  else
      (GetNodePeriod this_node other_nodes);

fun GetQueueFrontData [] = 0
  | GetQueueFrontData ({data=data, operation=operation}::other_opns) = data;
fun GetQueueFrontOperation ({data=data, operation=operation}::other_opns) = operation;

fun NoWaitingOperationsOnThisNode this_node this_time [] = true
  | NoWaitingOperationsOnThisNode this_node this_time ({node=node, enqueue_time=enqueue_time, data=data, operation=operation}::other_wopn) = 
      if (this_node = node) then 
        false
      else
        true andalso (NoWaitingOperationsOnThisNode this_node this_time other_wopn);

fun QueueOperationEnqueueTime {data=data, 
  operation={node=node, component=opn_component, operation=some_operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, data=some_data, steps=steps}} = 
    enqueue_time;

fun NetworkGuard clocks [] np wopn = false
  | NetworkGuard clocks [{node=node, dequeue_time=dequeue_time, queue=[]}] np wopn = false 
  | NetworkGuard clocks ({node=node, dequeue_time=dequeue_time, queue=queue}::other_nodes) np wopn =
  if (queue != [] andalso ((GetClockValue (hd (GetClockOnNode node clocks) )) >= (QueueOperationEnqueueTime (hd queue)) + 
             (GetDelay (GetQueueFrontData queue) (QueueOperationEnqueueTime (hd queue)) (GetNodePeriod node np) (GetNodeProfile node np))   )) then
      true
  else
      (false orelse (NetworkGuard clocks other_nodes np wopn)); 

fun SetEnqueueTime this_value {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, data=data, steps=steps} = 
    {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=this_value, data=data, steps=steps};

fun FindNextOperationOnThisNode this_node [] np = []
  | FindNextOperationOnThisNode this_node ({node=node, dequeue_time=dequeue_time, queue=queue}::other_nodes) np =
    if (this_node = node andalso queue != []) then
	     (* Add delay to dequeue time to get CMQ enqueue time *)
        if (dequeue_time >= (QueueOperationEnqueueTime (hd queue)) ) then 
	         [{node=node, enqueue_time = dequeue_time + (GetDelay (GetQueueFrontData queue) dequeue_time (GetNodePeriod this_node np) (GetNodeProfile this_node np)), 
               data=(GetQueueFrontData queue),
	             operation=(SetEnqueueTime (dequeue_time + (GetDelay (GetQueueFrontData queue) dequeue_time (GetNodePeriod this_node np) (GetNodeProfile this_node np)))
                  (GetQueueFrontOperation queue))}]
        else
           [{node=node, enqueue_time = (QueueOperationEnqueueTime (hd queue)) + 
                (GetDelay (GetQueueFrontData queue) (QueueOperationEnqueueTime (hd queue)) (GetNodePeriod this_node np) (GetNodeProfile this_node np)), 
               data=(GetQueueFrontData queue),
               operation=(SetEnqueueTime ((QueueOperationEnqueueTime (hd queue)) 
                          + (GetDelay (GetQueueFrontData queue) (QueueOperationEnqueueTime (hd queue)) (GetNodePeriod this_node np) (GetNodeProfile this_node np)))
                  (GetQueueFrontOperation queue))}]          
    else
	(FindNextOperationOnThisNode this_node other_nodes np);

fun UpdateNetworkQueues clocks [] np wopn = []
  | UpdateNetworkQueues clocks ({node=node, dequeue_time=dequeue_time, queue=queue}::other_nodes) np wopn =
  if ( (queue !=[] andalso (NetworkGuard clocks [{node=node, dequeue_time=dequeue_time, queue=queue}]) np wopn) = true) then
      if (dequeue_time >= (QueueOperationEnqueueTime (hd queue)) ) then 
        {node=node, dequeue_time = dequeue_time + 
                  (GetDelay (GetQueueFrontData queue) dequeue_time (GetNodePeriod node np) (GetNodeProfile node np)),
            queue=(tl queue)}
        ::(UpdateNetworkQueues clocks other_nodes np wopn)
      else
        {node=node, dequeue_time = (QueueOperationEnqueueTime (hd queue)) + 
                  (GetDelay (GetQueueFrontData queue) (QueueOperationEnqueueTime (hd queue)) (GetNodePeriod node np) (GetNodeProfile node np)),
            queue=(tl queue)}
        ::(UpdateNetworkQueues clocks other_nodes np wopn)        
  else
      {node=node, dequeue_time=dequeue_time, queue=queue}::(UpdateNetworkQueues clocks other_nodes np wopn);

fun NetworkDequeue [] network_queues np = []
  | NetworkDequeue ({node=node, value=value, next_tick=next_tick}::other_clocks) network_queues np = 
      (FindNextOperationOnThisNode node network_queues np)^^(NetworkDequeue other_clocks network_queues np);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* Get Delay Logger Function *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun DelayLogOnThisNode this_node [] np = []
  | DelayLogOnThisNode this_node ({node=node, dequeue_time=dequeue_time, queue=queue}::other_nodes) np =
    if (this_node = node andalso queue != []) then
  (* Add delay to dequeue time to get CMQ enqueue time *)
  [(GetDelay (GetQueueFrontData queue) dequeue_time (GetNodePeriod this_node np) (GetNodeProfile this_node np)), 
  dequeue_time + (GetDelay (GetQueueFrontData queue) dequeue_time (GetNodePeriod this_node np) (GetNodeProfile this_node np))]
    else
  (DelayLogOnThisNode this_node other_nodes np);

fun DelayLog [] network_queues np = []
   | DelayLog ({node=node, value=value, next_tick=next_tick}::other_clocks) network_queues np = 
      (DelayLogOnThisNode node network_queues np)^^(DelayLog other_clocks network_queues np);
