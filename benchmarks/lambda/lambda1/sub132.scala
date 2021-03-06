import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub132 {
  /*
  Name: Yoon Jae Nam (2012-81338)
  Organization: Seoul National University
  Class: Programming Language (4190.310)
  Problem: 8 (metro)
  */
  
  /* Provided type declarations */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  /* End of provided type declarations */
  
  /* nameExists: function that checks if (arg1 : name)
  exists in (arg2 : name list) */
  def nameExists: (Name, List[Name]) => Boolean = {
    case (name_foo, name_list) =>
      {
        name_list match {
          case Nil() => { false }
          case Cons(h, t) => {
            if (name_foo == h) true else nameExists(name_foo, t)
          }
        }
    }
  }
  /* end of nameExists */
  		
  /* addToNameList: adds (arg1 : name) to (arg2 : name list) if 
  arg1 doesn't already appear in arg2 */
  val addToNameList: (Name, List[Name]) => List[Name] = {
    case (name_foo, name_list) =>
      {
        
          if (
            nameExists(name_foo, name_list)
          ) {
            name_list 
          } else {
            name_foo :: name_list
          }
    }
  }
  /* end of addToNameList */
  
  /* checkMetroRec: helper function that has another argument of type 
  (name list). arg2 is the list of names that a station can have */
  def checkMetroRec: (Metro, List[Name]) => Boolean = {
    case (metro_foo, name_list) =>
      {
        metro_foo match {
          case STATION(name_bar) => { nameExists(name_bar, name_list) }
          case AREA(name_bar, metro_bar) => {
            checkMetroRec(metro_bar, addToNameList(name_bar, name_list))
          }
          case CONNECT(metro_left, metro_right) => {
            checkMetroRec(metro_left, name_list) &&
            checkMetroRec(metro_right, name_list)
          }
        }
    }
  }
  /* end of checkMetroRec */
  
  /* checkMetro function that checks for the correctness of a metro.
  Definition of the correctness of a metro (iff): The name of the station
  (id in STATION(id)) only appears in the area corresponding to that name 
  (m in AREA(id, m)) */
  val checkMetro: Metro => Boolean = ( (metro_foo) => { checkMetroRec(metro_foo, Nil()) } )
  
  /* Below: for testing */
  /*
  let printBool(bool_foo) =
  	if bool_foo = true then print_string "true"
  	else print_string "false"		
  
  let test_runner (test_name, test_metro, expected) =
  	let actual = checkMetro(test_metro) in
  		print_endline ("---------------");
  		if actual = expected then
  			(print_string ("Good (" ^ test_name ^ "): ");
  			printBool (actual);
  			print_newline ()
  			)
  		else
  			(print_endline ("***BAD (" ^ test_name ^ ")");
  			print_string "Expected: ";
  			printBool(expected);
  			print_newline ();
  			print_string "Actual: ";
  			printBool(actual);
  			print_newline ();
  			raise (Invalid_argument "test_runner"))
  
  let test1 =
  	let test_metro = AREA("a", STATION "a") in
  	let expected = true in
  	test_runner("test1", test_metro, expected)
  	
  let test2 =
  	let test_metro = AREA("a", AREA("a", STATION "a")) in
  	let expected = true in
  	test_runner("test2", test_metro, expected)
  	
  let test3 =
  	let test_metro = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))) in
  	let expected = true in
  	test_runner("test3", test_metro, expected)
  	
  let test4 =
  	let test_metro = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))) in
  	let expected = true in
  	test_runner("test4", test_metro, expected)
  	
  let test5 =
  	let test_metro = AREA("a", STATION "b") in
  	let expected = false in
  	test_runner("test5", test_metro, expected)
  	
  let test6 =
  	let test_metro = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))) in
  	let expected = false in
  	test_runner("test6", test_metro, expected)
  	
  let test7 =
  	let test_metro = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))) in
  	let expected = false in
  	test_runner("test7", test_metro, expected)
  
  let test8 =
  	let test_metro = STATION "a" in
  	let expected = false in
  	test_runner("test8", test_metro, expected)
  
  let test9 =
  	let test_metro = CONNECT(STATION "a", STATION "b") in
  	let expected = false in
  	test_runner("test9", test_metro, expected)
  
  let test10 =
  	let test_metro = AREA(("c"), CONNECT(AREA("b", AREA("a", STATION "b")), AREA("c", STATION "b"))) in
  	let expected = false in /* because of STATION "b" at the end */
  	test_runner("test10", test_metro, expected)
  
  let test11 =
  	let test_metro = AREA("b", AREA("c", CONNECT(AREA("b", AREA("a", STATION "b")), AREA("c", STATION "b")))) in
  	let expected = true in
  	test_runner("test11", test_metro, expected)
  /* end of test code */
  */
}