fun opns_string [] = ""
   | opns_string ({node=node ,component=component, operation=operation, enqueue_time=enqueue_time, completion_time=completion_time, deadline=deadline}::other_opns) = 
   (concat ["NODE=", node, "::COMPONENT=", component, "::OPERATION=", operation, "::ENQUEUE_TIME=", (Int.toString enqueue_time), "::COMPLETION_TIME=", (Int.toString completion_time), "::DEADLINE=", (Int.toString deadline), "::EXECUTION_TIME=", (Int.toString (completion_time-enqueue_time)), "\n", (opns_string other_opns)]);

val completed_operations = hd(Mark.Analysis_Model'Completed_Operations 1 (NoOfNodes()) );
val writestream = TextIO.openAppend "completed_operations.log";
TextIO.output(writestream, (opns_string completed_operations));
TextIO.closeOut writestream