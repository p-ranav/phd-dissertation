(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* Plexil Timing Analysis *)
(* Author: Pranav Srinivas Kumar *)
(* Date: 2016.02.25 *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)

(* Is it time to trigger an event? *)
fun EventGuard clock [] = false
   | EventGuard clock
({Event=Event, InterArrivalTime=InterArrivalTime, 
         NextArrival=NextArrival, Effect=Effect}::other_events) =
if (NextArrival = clock) then
     true
else
    false andalso (EventGuard clock other_events);

(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* Update Event Queue *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)

(* When an event occurs, 
   the event queue must be updated *)
fun UpdateEventQueue clock [] = []
   | UpdateEventQueue clock
({Event=Event, InterArrivalTime=InterArrivalTime, 
         NextArrival=NextArrival, Effect=Effect}::other_events) =
if (NextArrival = clock) then
     {Event=Event, ArrivalTime=clock}
::(UpdateEventQueue clock other_events)
else
    (UpdateEventQueue clock other_events);

(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* Update Events Place *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)    

(* When an event occurs, 
   the next arrival of the event must be updated *)
fun UpdateEvents clock [] = []
   | UpdateEvents clock
({Event=Event, InterArrivalTime=InterArrivalTime, 
         NextArrival=NextArrival, Effect=Effect}::other_events) =
if (NextArrival = clock) then
     {Event=Event, InterArrivalTime=InterArrivalTime, 
         NextArrival=NextArrival+InterArrivalTime, Effect=Effect}
  ::(UpdateEvents clock other_events)
else
    (UpdateEvents clock other_events);

(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* Update All Environment Variables *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)    

fun UpdateThisVariable VariableName NewValue [] = []
   | UpdateThisVariable VariableName NewValue 
    ({VariableName=CurrVariable,VariableType=VariableType,
          VariableValue=VariableValue, LookUpCost=LookUpCost}::other_variables) = 
    if (VariableName = CurrVariable) then
        ({VariableName=CurrVariable,VariableType=VariableType,
          VariableValue=NewValue, LookUpCost=LookUpCost}::other_variables)
    else
      {VariableName=CurrVariable,VariableType=VariableType,
          VariableValue=VariableValue, LookUpCost=LookUpCost}
      ::(UpdateThisVariable VariableName NewValue other_variables);

fun UpdateVariablesThisEvent [] variables = variables
  | UpdateVariablesThisEvent ({VariableName=VariableName, NewValue=NewValue}::other_effects) variables = 
      (UpdateVariablesThisEvent other_effects (UpdateThisVariable VariableName NewValue variables));

fun UpdateAllVariables clock [] variables = variables
  | UpdateAllVariables clock 
       ({Event=Event, InterArrivalTime=InterArrivalTime, 
         NextArrival=NextArrival, Effect=Effect}::other_events)
         variables = 
    if (NextArrival = clock) then
      (UpdateAllVariables clock other_events (UpdateVariablesThisEvent Effect variables))
    else
      (UpdateAllVariables clock other_events variables);

(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* ExecuteGuard *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*) 

fun CanNodesExecute [] = false
  | CanNodesExecute
  ({NodeType=NodeType, NodeID=NodeID, State=State,
        Parent=Parent, VariableList=VariableList, 
        ConditionList=ConditionList, AssignmentList=AssignmentList, 
        CommandList=CommandList, Outcome=Outcome}::other_nodes) = 
  if (Parent="NULL" andalso State="Inactive") then
    true 
  else
    false orelse (CanNodesExecute other_nodes);

fun AreThereEventsToProcess event_queue = 
  if (event_queue != []) then true else false;

fun ExecuteGuard 
  plexil_nodes
  event_queue = 
  if ((CanNodesExecute plexil_nodes) andalso
      (AreThereEventsToProcess event_queue)) then true else false;

(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* Execute Plexil Nodes *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)

fun IsRootNode {NodeType=NodeType, 
                     NodeID=NodeID, State=State,
                     Parent=Parent, 
                     VariableList=VariableList,
                     ConditionList=ConditionList,
                     AssignmentList=AssignmentList,
                     CommandList=CommandList, 
                     Outcome=Outcome}  = 
      if (Parent = "NULL") then true else false;   

fun CheckConditions {NodeType=NodeType, 
                     NodeID=NodeID, State=State,
                     Parent=Parent, 
                     VariableList=VariableList,
                     ConditionList=ConditionList,
                     AssignmentList=AssignmentList,
                     CommandList=CommandList, 
                     Outcome=Outcome}  = 
      case NodeID of
        "DriveToRedRock1" => true
      | "SenseRR" => true
      | _ => true;

fun ExecuteThisNode {NodeType=NodeType, 
                     NodeID=NodeID, State=State,
                     Parent=Parent, 
                     VariableList=VariableList,
                     ConditionList=ConditionList,
                     AssignmentList=AssignmentList,
                     CommandList=CommandList, 
                     Outcome=Outcome} environment_variables = 
                 {NodeType=NodeType, 
                     NodeID=NodeID, State=State,
                     Parent=Parent, 
                     VariableList=VariableList,
                     ConditionList=ConditionList,
                     AssignmentList=AssignmentList,
                     CommandList=CommandList, 
                     Outcome=Outcome};

fun ExecuteNodes [] environment_variables executed_nodes = []
  | ExecuteNodes 
  (first_node::other_nodes)
  environment_variables
  executed_nodes = 
    if ((IsRootNode first_node = true) andalso (CheckConditions first_node = true)) then
      (ExecuteThisNode first_node environment_variables)::(ExecuteNodes other_nodes environment_variables (executed_nodes^^[first_node]))
    else
      (other_nodes);

      (*

-----Original Appointment-----
From: gabor.karsai@Vanderbilt.Edu [mailto:gabor.karsai@Vanderbilt.Edu] On Behalf Of ARC-CR-N269-R284
Sent: Thursday, January 14, 2016 2:57 PM
To: ARC-CR-N269-R284; Gabor Karsai
Subject: FW: AOS Weekly Tag
When: Monday, March 07, 2016 12:00 PM-1:30 PM (UTC-06:00) Central Time (US & Canada).
Where: N269/284


 
 
-----Original Appointment-----
From: Bajwa, Anupa R. (ARC-TI) [mailto:anupa.r.bajwa@nasa.gov] On Behalf Of ARC-CR-N269-R284
Sent: Friday, October 30, 2015 3:36 PM
To: ARC-CR-N269-R284; Lowry, Michael R. (ARC-TI); Knudson, Matt (ARC-TI); CASTLE, JOSEPH P. (ARC-TI); Dalal, Michael (ARC-TI)[SGT, INC]; Quach, Cuong Chi (LARC-D320); Cooper, Eric G. (LARC-D320); Eure, Kenneth W. (LARC-D320); Beaton, Brian F. (LARC-D101); Karsai, Gabor; Biatek, Jason T. (ARC-TI)[Regents of the University of Minnesota]; rsanjai@cs.umn.edu; Bajwa, Anupa R. (ARC-TI)
Subject: FW: AOS Weekly Tag
When: Occurs every Monday effective 11/2/2015 from 10:00 AM to 11:30 AM (UTC-08:00) Pacific Time (US & Canada).
Where: N269/284
 
 
Starting at 9:45 am on Monday, November 2: Team tag-up followed by a presentation by Professor Karsai on template for component models, generation of scheduling logic, and Colored Petri Nets.
 
 
New location : N269 Room 284
 
AOS weekly tagup on Mondays at 10:00 a.m. Pacific time.
 
Telecon number: 844-467-6272
Participant passcode: 167280
 
-----Original Appointment-----
From: ARC-CR-N269-R284 
Sent: Monday, October 26, 2015 1:13 PM
To: ARC-CR-N269-R284; Bajwa, Anupa R. (ARC-TI)
Subject: AOS Weekly Tag
When: Occurs every Monday effective 11/2/2015 from 10:00 AM to 11:30 AM (UTC-08:00) Pacific Time (US & Canada).
Where: N269/284
 

        *)
      
