(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* WORST-CASE TRIGGER-TO-RESPONSE TIME ESTIMATION *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  

(* TRIGGER TO RESPONSE TIME ESTIMATION *)
fun Completed_Operations_TRT n = (Mark.Analysis_Model'Completed_Operations 1 n <> []);

fun get_opname {node=node, component=component, operation=operation, enqueue_time=enqueue_time, completion_time=completion_time, deadline=deadline} 
                                = operation;

fun is_op_complete opname cop = ((get_opname cop) = opname);

val cop_nodes = SearchNodes (
        EntireGraph,
        fn n => (Completed_Operations_TRT n),
       	NoLimit,
       	fn n => n,
       	[],
       	op ::);

val All_Completed = sort INT.lt cop_nodes;
fun get_cop_list cop_node = (hd (Mark.Analysis_Model'Completed_Operations 1 cop_node));

fun find_op op_id [] = {node="", component="", operation="", enqueue_time=0, completion_time=0, deadline=0}
   |  find_op op_id (cop::cop_list) = 
            if (is_op_complete op_id cop) then cop
            else (find_op op_id cop_list);

fun found_op op_id [] = false
    |  found_op op_id (cop::cop_list) = 
         if (is_op_complete op_id cop) then true
              else false orelse (found_op op_id cop_list);

fun Search_cop_nodes op_id [] = {node="", component="", operation="", enqueue_time=0, completion_time=0, deadline=0}
   | Search_cop_nodes op_id (cop_node::rest_cop_nodes) = 
     if (found_op op_id (get_cop_list cop_node)) then 
              (find_op op_id (get_cop_list cop_node))
      else (Search_cop_nodes op_id rest_cop_nodes);

fun calculate_response_time {node=trigger_node, component=trigger_component, operation=trigger_operation, 
									enqueue_time=trigger_enqueue_time, completion_time=trigger_completion_time, deadline=trigger_deadline} 
                          {node=response_node, component=response_component, operation=response_operation, 
                          			enqueue_time=response_enqueue_time, completion_time=response_completion_time, deadline=response_deadline}  = 
     (response_completion_time - trigger_enqueue_time);
 
fun TriggerToResponseTime trigger_operation response_operation = 
  (write_to_file [(concat ["\nTRIGGER TO RESPONSE TIME ESTIMATION: \n", 
                           "Trigger Operation: ", trigger_operation, "; Reponse Operation: ", response_operation,
                           "; Trigger to Response Time:  ", 
                          (Int.toString (calculate_response_time (Search_cop_nodes trigger_operation All_Completed) 
                                                 (Search_cop_nodes response_operation All_Completed))
                          ), 
                          " us"])] 
  "Analysis_Report.log");     