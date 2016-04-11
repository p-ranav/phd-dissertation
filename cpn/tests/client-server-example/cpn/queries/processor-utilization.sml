(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* PROCESSOR UTILIZATION ESTIMATION *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  

(* CPU UTILIZATION ESTIMATION - HELPER FUNCTIONS *)
val length = NoOfNodes();
val Completed_Operations_CPU_UTIL = hd (Mark.Analysis_Model'Completed_Operations 1 length);

fun compute_requirement [] = 0
   | compute_requirement ({node=node, component=component, operation=operation, enqueue_time=enqueue_time, completion_time=completion_time, deadline=deadline}::other_completed_operations) = 
       if (opname = "Sensor_Read") then
              (17000 + (compute_requirement other_completed_operations))
       else 
         if (opname = "Control_Loop") then
             (48025 + (compute_requirement other_completed_operations))
       else
              (8439 + (compute_requirement other_completed_operations));

fun compute_capacity ({node=node, component=component, operation=operation, enqueue_time=enqueue_time, completion_time=completion_time, deadline=deadline}::other_completed_operations) = 
       if (other_completed_operations = []) then 
            op_et
       else
         (compute_capacity other_completed_operations);

