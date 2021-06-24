type record_type =
    { field1: char;
      field2: string
};;
let record_value = 
    { field1 = 'a';
      field2 = "abc"
};;
let record_value_with_mixed_parameters = 
    { field2 = "def";
      field1 = 'b'
};;
let record_copie = 
  { record_value with 
        field1 = 'c'};;

type mutable_record_type =
  { normal_field: char;
    mutable mutable_field: string
  };;
let mutable_record_value = 
  { normal_field = 'a';
    mutable_field = "abc"
  };;
let mutable_record_assign_to_mutable = 
  mutable_record_value.mutable_field <- 
    mutable_record_value.mutable_field ^ "def";;

type record_one_line = {test: string; test2 : int};;
let record_one_line_value = {test = "abc"; test2 = 1};;

type user =
    { port: int;
      user: string;
      credentials: string;
      mutable last_heartbeat_time: int;
      mutable last_heartbeat_status: string;
};;
type user2 =
    { port2: int;
      user2: string;
      credentials2: string;
      mutable last_heartbeat_time2: int;
      mutable last_heartbeat_status2: string;
};;

let user_value = 
    { port = 1 + 2;
      user = "user" ^ "123";
      credentials = "abc" ^ "def" ^ "hij" ^ "klm" ^ "nop";
      last_heartbeat_time = 0; 
      last_heartbeat_status = "abc"
};;

let user_copy record = 
    { record with 
        port = 400 * 2 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        last_heartbeat_time = record.last_heartbeat_time + 1 };;

let user_assign_to_mutable record new_status = 
    record.last_heartbeat_time <- record.last_heartbeat_time + 1;
    record.last_heartbeat_status <- new_status;;
