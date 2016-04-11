(* INTEGRATED TIMING ANALYSIS AND VERIFICATION OF COMPONENT-BASED DISTRIBUTED REAL-TIME EMBEDDED SYSTEMS *)
(* AUTHOR: PRANAV SRINIVAS KUMAR *)

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(* WRITE STRING TO FILE *)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*) 

fun write_to_file [] file_path = 
   let
       val writestream = TextIO.openAppend file_path
   in 
       (TextIO.closeOut writestream)
   end
| write_to_file (first_string::rest) file_path = 
    let 
       val writestream = TextIO.openAppend file_path
       fun write(message) = TextIO.output(writestream, concat["\n", message])
    in
       write(first_string); (write_to_file rest file_path)
    end;