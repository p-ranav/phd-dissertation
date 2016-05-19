(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* GENERIC HELPER FUNCTIONS *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)  

(* Find minimum integer in a list of integers *)
fun FindMinimum [] = ~1
  | FindMinimum [number] = number
  | FindMinimum (number::other_numbers) = 
  	if (number < (FindMinimum other_numbers)) then 
  		number
  	else
  		(FindMinimum other_numbers);

(* Delete a given number of a list *)
fun DeleteNumber this_number [] = []
  | DeleteNumber this_number (number::other_numbers) = 
  		if (number = this_number) then 
  			(DeleteNumber this_number other_numbers)
  		else
  			number::(DeleteNumber this_number other_numbers);