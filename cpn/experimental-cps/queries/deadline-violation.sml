(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* DEADLINE VIOLATION DETECTION *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  

val length = NoOfNodes();
val Completed_Operations_DL = hd (Mark.Analysis_Model'Completed_Operations 1 length);

fun deadline_violation_check [] = []
   | deadline_violation_check ({node=node, component=component, operation=operation, enqueue_time=enqueue_time, completion_time=completion_time, deadline=deadline}::other_operations) = 
       if (completion_time - enqueue_time > deadline) then
              ({node=node, component=component, operation=operation, enqueue_time=enqueue_time, completion_time=completion_time, deadline=deadline}::(deadline_violation_check other_operations))
       else
             (deadline_violation_check other_operations);

fun print_dl [] = [] 
  | print_dl ({node=node, component=component, operation=operation, enqueue_time=enqueue_time, completion_time=completion_time, deadline=deadline}::other_operations) = 
    (concat ["Node: ", node,
    		 "; Component: ", component,
    		 "; Operation: ", operation, 
    		 "; Enqueue Time: ", (Int.toString enqueue_time),
    		 " us; Completion Time: ", (Int.toString completion_time),
    		 " us; Execution Time: ", (Int.toString (completion_time - enqueue_time)), 
    		 " us; Deadline: ", (Int.toString deadline), " us"])::(print_dl other_operations);

(* All Deadline Violations *)
val deadline_violations = (print_dl (deadline_violation_check Completed_Operations_DL));
(write_to_file ["ALL DEADLINE VIOLATIONS:"] "Analysis_Report.log");
(write_to_file deadline_violations "Analysis_Report.log");    

(* Detect First Deadline Violation *)
val first_deadline_violation = print_dl [hd (deadline_violation_check Completed_Operations_DL)];
(write_to_file ["\nFIRST DEADLINE VIOLATION:"] "Analysis_Report.log");
(write_to_file first_deadline_violation "Analysis_Report.log");