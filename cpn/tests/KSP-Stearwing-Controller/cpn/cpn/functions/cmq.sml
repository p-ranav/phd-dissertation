(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* ENQUEUE OPERATION BASED ON SCHEDULING SCHEME *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

(* Prepare Operation for Execution *)
fun EnqueueHelper {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}
                      scheme
                      [] = [{node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}]
  | EnqueueHelper {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}
                      scheme
                      ({node=node2, component=opn_component2, operation=operation2, priority=priority2, deadline=deadline2, enqueue_time=enqueue_time2, steps=steps2}::other_opns) = 
      case scheme 
      of FIFO =>
        ({node=node2, component=opn_component2, operation=operation2, priority=priority2, deadline=deadline2, enqueue_time=enqueue_time2, steps=steps2}::other_opns)^^
         [{node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}]
      | PFIFO =>
        if (priority > priority2) then 
          ({node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}::
           {node=node2, component=opn_component2, operation=operation2, priority=priority2, deadline=deadline2, enqueue_time=enqueue_time2, steps=steps2}::other_opns)
        else
          {node=node2, component=opn_component2, operation=operation2, priority=priority2, deadline=deadline2, enqueue_time=enqueue_time2, steps=steps2}::
          (EnqueueHelper {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps} scheme other_opns)
      | EDF =>
        if (deadline < deadline2) then 
          ({node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}::
           {node=node2, component=opn_component2, operation=operation2, priority=priority2, deadline=deadline2, enqueue_time=enqueue_time2, steps=steps2}::other_opns)
        else
          {node=node2, component=opn_component2, operation=operation2, priority=priority2, deadline=deadline2, enqueue_time=enqueue_time2, steps=steps2}::
          (EnqueueHelper {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps} scheme other_opns);

(* Enqueue Operation onto CMQ *)
fun Enqueue operation {component=component, scheme=scheme, queue=queue} = 
    {component=component, scheme=scheme, queue=(EnqueueHelper operation scheme queue)};

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* FIND ENQUEUE SPOT *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

(* Get list of operations on a specific node i.e. Subset based on Node Name *)  
fun GetOperationsByName this_node [] = []
  | GetOperationsByName this_node
     ({node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}::other_opns) = 
     if (this_node = node) then
       ({node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}::
        (GetOperationsByName this_node other_opns))
    else
      (GetOperationsByName this_node other_opns); 

(* Find right CMQ and enqueue *)
fun FindCMQAndEnqueue {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps} [] = []
  | FindCMQAndEnqueue {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}
                      ({component=component, scheme=scheme, queue=queue}::other_cmqs) = 
      if (opn_component = component) then 
          (Enqueue {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}
                                       {component=component, scheme=scheme, queue=queue})::other_cmqs
      else
          {component=component, scheme=scheme, queue=queue}::
          (FindCMQAndEnqueue {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}
                             other_cmqs);

(* Find CMQ Node and enqueue *)
fun FindCMQNodeAndEnqueue ({node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}::other_opns) 
                      [] = []
  | FindCMQNodeAndEnqueue [] [] = []
  | FindCMQNodeAndEnqueue [] ({node=cmq_node, cmql=cmql}::other_nodes) = ({node=cmq_node, cmql=cmql}::other_nodes)
  | FindCMQNodeAndEnqueue ({node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}::other_opns)
                     ({node=cmq_node, cmql=cmql}::other_nodes) = 
      if (node = cmq_node) then
          (FindCMQNodeAndEnqueue other_opns ({node=cmq_node, cmql=(FindCMQAndEnqueue 
                  {node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps} cmql)}::other_nodes))
      else
          (FindCMQNodeAndEnqueue other_opns 
            ({node=cmq_node, cmql=cmql}
          ::(FindCMQNodeAndEnqueue ({node=node, component=opn_component, operation=operation, priority=priority, deadline=deadline, enqueue_time=enqueue_time, steps=steps}::other_opns)
                                   other_nodes)));    

(* Enqueue all operations - when provided with a list of operations *)
fun EnqueueAllOperations [] node_cmqs = []
  | EnqueueAllOperations (opn::other_opns) node_cmqs = 
       (FindCMQNodeAndEnqueue (opn::other_opns) node_cmqs);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* RETURN CMQ LIST ON NODE *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  

(* Given a node, give me the node cmq from the node cmq list *)
(* CAUTION - This returns a list - Take the head *)
fun GetCMQOnNode node [] = []
  | GetCMQOnNode node ({node=cmq_node, cmql=cmql}::other_nodes) = 
  if (cmq_node = node) then 
    [{node=cmq_node, cmql=cmql}]
  else 
    (GetCMQOnNode node other_nodes);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* FIND ALL INTERACTIONS WAITING TO ENQUEUE ON A GIVEN NODE *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  
fun GetInteractionsOnNode this_node [] = []
  | GetInteractionsOnNode this_node ({node=node, enqueue_time=enqueue_time, operation=operation}::other_interactions) = 
      if (node=this_node) then
        {node=node, enqueue_time=enqueue_time, operation=operation}::(GetInteractionsOnNode this_node other_interactions)
      else
        (GetInteractionsOnNode this_node other_interactions);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* FIND THE NEXT CLOSEST ENQUEUE TIME *)
(* A COMPONENT OPERATION NEEDS TO BE ENQUEUED ONTO SOME QUEUE AT THIS TIME STAMP AND SO THIS POINT CANNOT BE ACCIDENTALLY SKIPPED DURING "PROGRESS_TIME" *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)         
fun NextInteractionEnqueueTime [] = ~1
  | NextInteractionEnqueueTime [{node=node, enqueue_time=enqueue_time, operation=operation}] = enqueue_time
  | NextInteractionEnqueueTime ({node=node, enqueue_time=enqueue_time, operation=operation}::
                                {node=node2, enqueue_time=enqueue_time2, operation=operation2}::other_interactions) = 
        if (enqueue_time <= enqueue_time2) then 
          (NextInteractionEnqueueTime ({node=node, enqueue_time=enqueue_time, operation=operation}::other_interactions))
        else 
          (NextInteractionEnqueueTime ({node=node2, enqueue_time=enqueue_time2, operation=operation2}::other_interactions));

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* OPERATION ENQUEUE GUARD - CHECK IF ANY WAITING OPERATIONS CAN BE ENQUEUED AT THE CURRENT POINT IN TIME *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)   
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  

fun CanEnqueueThisOperation {node=clock_node, value=value, next_tick=next_tick}  
                            {node=node, enqueue_time=enqueue_time, operation=operation} = 
            if (value = enqueue_time) then 
              true
            else
              false       

fun EnqueueGuard clocks [] = false
  | EnqueueGuard clocks ({node=node, enqueue_time=enqueue_time, operation=operation}::other_waiting_operations) = 
      if ((CanEnqueueThisOperation (hd (GetClockOnNode node clocks)) {node=node, enqueue_time=enqueue_time, operation=operation}) = true) then
        true
      else 
        false orelse (EnqueueGuard clocks other_waiting_operations);

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* GET ALL WAITING OPERATIONS *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)   
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)          

fun GetOperationsList clocks [] = []
  | GetOperationsList clocks ({node=node, enqueue_time=enqueue_time, operation=operation}::other_waiting_operations) = 
        if (EnqueueGuard clocks [{node=node, enqueue_time=enqueue_time, operation=operation}]) then 
          operation::(GetOperationsList clocks other_waiting_operations)
        else
          (GetOperationsList clocks other_waiting_operations);

fun RemoveOperations clocks [] = []
  | RemoveOperations clocks ({node=node, enqueue_time=enqueue_time, operation=operation}::other_waiting_operations) = 
        if (EnqueueGuard clocks [{node=node, enqueue_time=enqueue_time, operation=operation}]) then 
            (RemoveOperations clocks other_waiting_operations)
        else
            {node=node, enqueue_time=enqueue_time, operation=operation}::(RemoveOperations clocks other_waiting_operations);


(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* Record Queue Sizes in Queue Observer *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)   
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)

fun RecordQueueSizeThisNode clocks {node=node, cmql=[]} = []
  | RecordQueueSizeThisNode clocks {node=node, cmql=({component=component, scheme=scheme, queue=queue}::other_cmq)} =
    {node=node, component=component, clock=(GetClockValue (hd (GetClockOnNode node clocks))), queue_size=(List.length queue)}
    ::(RecordQueueSizeThisNode clocks {node=node, cmql=other_cmq});

fun RecordQueueSizes clocks [] = []
  | RecordQueueSizes clocks ({node=node, cmql=cmql}::other_nodes) =
  (RecordQueueSizeThisNode clocks {node=node, cmql=cmql})^^(RecordQueueSizes clocks other_nodes); 
  
