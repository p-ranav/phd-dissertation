(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* Plexil Timing Analysis *)
(* Author: Pranav Srinivas Kumar *)
(* Date: 2016.04.06 *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)

(* Helper Function to remove element from list *)
fun RemoveElem elem myList = filter (fn x => x <> elem) myList;

(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* SHOULD WE UPDATE THE FIRING TIME OF THE EVENT? *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
fun FiringGuard clock {name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time} = 
	if (clock >= firing_time) then true else false;

fun UpdateFiringGuard clock [] = false
  | UpdateFiringGuard clock (event::other_events) = 
  		if ((FiringGuard clock event) = true) then true
  		else false orelse (UpdateFiringGuard clock other_events);

fun CalculateFiringTime clock {name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time} = 
	if (clock >= firing_time) then
		{name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time + (Real.floor (uniform((Real.fromInt min_iat), (Real.fromInt max_iat))))}
	else	
		{name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time};

fun UpdateFiringTime clock [] = []
  | UpdateFiringTime clock (event::other_events) = 
		(CalculateFiringTime clock event)::(UpdateFiringTime clock other_events);
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* EVENT FIRING GUARD - IS ANY EVENT IN THE EVENTS_LIST READY TO FIRE? *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
fun EventGuard clock {name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time} = 
	if (clock < firing_time) then
		true
	else
		false;	

fun EventsGuard clock [] = false
  | EventsGuard clock (event::other_events) = 
  		if ((EventGuard clock event) = true) then
  			true
  		else
  			false orelse (EventsGuard clock other_events);

(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* FIND EARLIEST EVENT AND UPDATE CLOCK TO THIS VALUE *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
fun EarliestEvent [{name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time}] = firing_time
  | EarliestEvent ({name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time}
  ::{name=name2, min_iat=min_iat2, max_iat=max_iat2, updates=({name=var_name2, new_value=var_value2}::other_updates2), firing_time=firing_time2}::other_events) = 
  if (firing_time < firing_time2) then 
  (EarliestEvent ({name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time}::other_events))
  else
  (EarliestEvent ({name=name2, min_iat=min_iat2, max_iat=max_iat2, updates=({name=var_name2, new_value=var_value2}::other_updates2), firing_time=firing_time2}::other_events));

(* Update by shortest time *)
fun UpdateClock clock events = (EarliestEvent events);

(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* ENQUEUE EARLIEST EVENT ONTO EVENT QUEUE *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
fun FindEarliestEvent [{name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time}] = 
		[{name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time}]
  | FindEarliestEvent ({name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time}
  ::{name=name2, min_iat=min_iat2, max_iat=max_iat2, updates=({name=var_name2, new_value=var_value2}::other_updates2), firing_time=firing_time2}::other_events) = 
  if (firing_time < firing_time2) then 
  (FindEarliestEvent ({name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time}::other_events))
  else
  (FindEarliestEvent ({name=name2, min_iat=min_iat2, max_iat=max_iat2, updates=({name=var_name2, new_value=var_value2}::other_updates2), firing_time=firing_time2}::other_events));

fun EnqueueEvent event_queue event_list = 
	event_queue^^(FindEarliestEvent event_list);

(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* UPDATE FIRING TIME OF ENQUEUED EVENT *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
fun SameEvent {name=name, min_iat=min_iat, max_iat=max_iat, updates=({name=var_name, new_value=var_value}::other_updates), firing_time=firing_time}
  {name=name2, min_iat=min_iat2, max_iat=max_iat2, updates=({name=var_name2, new_value=var_value2}::other_updates2), firing_time=firing_time2} = 
      if (name = name2 andalso firing_time=firing_time2) then true else false;
 
fun UpdateEventList clock earliest_event [] = []
  | UpdateEventList clock earliest_event (event::event_list) = 
    if ( (SameEvent event earliest_event) = true) then
    (CalculateFiringTime (UpdateClock clock [earliest_event]) event)::(UpdateEventList clock earliest_event event_list)
    else
     event::(UpdateEventList clock earliest_event event_list);

(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
(* UPDATING VARIABLE LIST WHEN EVENT FIRES *)
(*--------------------------------------------------------------------*)
(*--------------------------------------------------------------------*)
fun UpdateThisVariable {name=var_name, value=var_value} [] = []
  | UpdateThisVariable {name=var_name, value=var_value} ({name=name, value=value}::other_variables) = 
    if (var_name = name) then 
        {name=var_name, value=var_value}::other_variables
    else
        {name=name, value=value}::(UpdateThisVariable {name=var_name, value=var_value} other_variables);

fun UpdateVariableList {name=name, min_iat=min_iat, max_iat=max_iat, 
          updates=[], firing_time}
          variable_list = variable_list

  | UpdateVariableList {name=name, min_iat=min_iat, max_iat=max_iat, 
          updates=({name=var_name, new_value=var_value}::other_updates), firing_time}

          variable_list = 

    (UpdateVariableList {name=name, min_iat=min_iat, max_iat=max_iat, updates=other_updates, firing_time=firing_time} 
        (UpdateThisVariable {name=var_name, value=var_value} variable_list));



