let simple_sequence = print_endline "First"; print_endline "Second";;

let begin_end = 
  begin
    print_endline "First";
    print_endline "Second"
  end;;

let parenthesis = 
  (
    print_endline "First";
    print_endline "Second"
  );;

let nested_sequences = 
  begin
    print_endline "First";
    begin
      print_endline "Second";
      print_endline "Third"
    end
  end;;

let if_then = 
  if true then (
    print_endline "First";
    print_endline "Second"
  );;

type record_type =
{ user: string;
  mutable last_heartbeat_time: int;
  mutable last_heartbeat_status: string;
};;
let sequence_for_records record new_status = 
    record.last_heartbeat_time <- record.last_heartbeat_time + 1;
    record.last_heartbeat_status <- new_status;;