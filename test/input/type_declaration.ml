type alias = char;;

type simple = A | B
and with_data = 
  | A2 
  | B2 of int 
  | C2 of char * int;;

type 'a polymorphic = 
  | A3 
  | B3 of 'a * 'a polymorphic;;

type 'a record = 
  {first : 'a list;
   mutable second : char};;

