(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* SENSOR GUARD *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  

(* Check for sensor expiry *)
fun SensorExpired {node=node, value=value, next_tick=next_tick} [] = false
  | SensorExpired {node=node, value=value, next_tick=next_tick} 
                 ({node=sensor_node, name=name, period=period, offset=offset}::other_sensors) = 
         if (node = sensor_node andalso value = offset) then 
			true
		 else (SensorExpired {node=node, value=value, next_tick=next_tick} other_sensors);   


(* Guard for Sensor_Expiry Transition *)  	
fun SensorExpiryGuard [] [] = false
  | SensorExpiryGuard [] sensors = false
  | SensorExpiryGuard (clock::other_clocks) [] = false
  | SensorExpiryGuard (clock::other_clocks) sensors = 
       if (LimitReached (clock::other_clocks) = false) then 
     	  if (SensorExpired clock sensors = true) then
     	     true
     	  else 
     	  	 (SensorExpiryGuard other_clocks sensors)
       else
          false; 

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* RETURN LIST OF SENSOR OPERATIONS *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)            

(* Get Sensors *)
fun GetSensors {node=node, value=value, next_tick=next_tick} [] = []
  | GetSensors {node=node, value=value, next_tick=next_tick}
                       ({node=sensor_node, name=name, period=period, offset=offset}::other_sensors) = 
            if (node = sensor_node andalso value = offset) then 
            ({node=sensor_node, name=name, period=period, offset=offset}
            ::(GetSensors {node=node, value=value, next_tick=next_tick} other_sensors))
            else 
            (GetSensors {node=node, value=value, next_tick=next_tick} other_sensors); 

(* Find operations to prepare for enqueue *)
fun ListSensorOperations [] sensors = []
 | ListSensorOperations (clock::other_clocks) sensors = 
      (GetSensors clock sensors)^^(ListSensorOperations other_clocks sensors); 

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* UPDATE TIMER OFFSETS AFTER TIMER EXPIRY IS HANDLED *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)         

(* Update Sensor Offsets *)
fun UpdateSensors clocks [] = []
  | UpdateSensors clocks ({node=sensor_node, name=name, period=period, offset=offset}::other_sensors) = 
         if ((SensorExpiryGuard clocks [{node=sensor_node, name=name, period=period, offset=offset}]) = true) then
         	({node=sensor_node, name=name, period=period, offset=offset+period}::(UpdateSensors clocks other_sensors))
         else
         	({node=sensor_node, name=name, period=period, offset=offset}::(UpdateSensors clocks other_sensors));

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* GET ALL SENSORS ON A GIVEN NODE *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)           
fun GetSensorsOnNode this_node [] = []
  | GetSensorsOnNode this_node ({node=node, name=name, period=period, offset=offset}::other_sensors) = 
      if (node=this_node) then 
        {node=node, name=name, period=period, offset=offset}::(GetSensorsOnNode this_node other_sensors)
      else
        (GetSensorsOnNode this_node other_sensors); 

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* FIND NEXT SENSOR OFFSET - TIME STAMP WHEN NEXT CLOSEST SENSOR WILL UPDATE *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)   
fun NextSensorOffset [] = ~1
  | NextSensorOffset [{node=node, name=name, period=period, offset=offset}] = offset
  | NextSensorOffset ({node=node, name=name, period=period, offset=offset}::
                     {node=node2, name=name2, period=period2, offset=offset2}::other_sensors) = 
      if (offset <= offset2) then 
        (NextSensorOffset ({node=node, name=name, period=period, offset=offset}::other_sensors))
      else
        (NextSensorOffset ({node=node2, name=name2, period=period2, offset=offset2}::other_sensors));
