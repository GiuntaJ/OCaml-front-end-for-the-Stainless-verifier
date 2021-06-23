import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub144 {
  /*
  Name: Yoon Jae Nam (2012-81338)
  Organization: Seoul National University
  Class: Programming Language (4190.310)
  Problem: 6: true or false
  */
  
  /* Provided type declarations */
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
  /* end of provided types */
  
  /* evalExpr evalutes an expr to an integer */
  def evalExpr: Expr => Int63 = (
    (expr_foo) =>
      {
        expr_foo match {
          case NUM(num) => { num }
          case PLUS(e_left, e_right) => { evalExpr(e_left) + evalExpr(e_right) }
          case MINUS(e_left, e_right) => { evalExpr(e_left) - evalExpr(e_right)
          }
        }
    }
  )
  /* end of evalExpr */
  
  /* eval function that evaluates a formula to a boolean value */
  def eval: Formula => Boolean = (
    (formula_foo) =>
      {
        formula_foo match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(formula_bar) => { not(eval(formula_bar)) }
          case ANDALSO(f_left, f_right) => {
            val _14 = {
              val bool_left = eval(f_left)
              val _15 = {
                val bool_right = eval(f_right)
                (bool_left, bool_right) match {
                  case (true, true) => { true }
                  case (_, _) => { false }
                }
              }
            }
          }
          case ORELSE(f_left, f_right) => {
            val _10 = {
              val bool_left = eval(f_left)
              val _11 = {
                val bool_right = eval(f_right)
                (bool_left, bool_right) match {
                  case (false, false) => { false }
                  case (_, _) => { true }
                }
              }
            }
          }
          case IMPLY(f_left, f_right) => {
            val _6 = {
              val bool_left = eval(f_left)
              val _7 = {
                val bool_right = eval(f_right)
                (bool_left, bool_right) match {
                  case (false, _) => { true }
                  case (true, true) => { true }
                  case (true, false) => { false }
                }
              }
            }
          }
          case LESS(e_left, e_right) => {
            val _2 = {
              val int_left = evalExpr(e_left)
              val _3 = {
                val int_right = evalExpr(e_right)
                if (int_left < int_right) true else false
              }
            }
          }
        }
    }
  )
  	
  /* Below: for testing */
  /*
  let printBool (bool_foo) =
  	if bool_foo = true then print_string "true"
  	else print_string "false"		
  
  let test_runner (test_name, test_formula, expected) =
  	let actual = eval(test_formula) in
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
  	let test_formula = TRUE in
  	let expected = true in
  	test_runner("TRUE", test_formula, expected)
  
  let test2 =
  	let test_formula = FALSE in
  	let expected = false in
  	test_runner("FALSE", test_formula, expected)
  
  let test3 =
  	let test_formula = NOT TRUE in
  	let expected = false in
  	test_runner("NOT TRUE", test_formula, expected)
  
  let test4 =
  	let test_formula = NOT FALSE in
  	let expected = true in
  	test_runner("NOT FALSE", test_formula, expected)
  
  let test5 =
  	let test_formula = NOT (NOT TRUE) in
  	let expected = true in
  	test_runner("NOT (NOT TRUE)", test_formula, expected)
  
  let test6 =
  	let test_formula = ANDALSO (TRUE, TRUE) in
  	let expected = true in
  	test_runner("ANDALSO (TRUE, TRUE)", test_formula, expected)
  
  let test7 =
  	let test_formula = ANDALSO (TRUE, FALSE) in
  	let expected = false in
  	test_runner("ANDALSO (TRUE, FALSE)", test_formula, expected)
  
  let test8 =
  	let test_formula = ANDALSO (FALSE, TRUE) in
  	let expected = false in
  	test_runner("ANDALSO (FALSE, TRUE)", test_formula, expected)
  
  let test9 =
  	let test_formula = ANDALSO (FALSE, FALSE) in
  	let expected = false in
  	test_runner("ANDALSO (FALSE, FALSE)", test_formula, expected)
  
  let test10 =
  	let test_formula = ORELSE (TRUE, TRUE) in
  	let expected = true in
  	test_runner("ORELSE (TRUE, TRUE)", test_formula, expected)
  
  let test11 =
  	let test_formula = ORELSE (FALSE, TRUE) in
  	let expected = true in
  	test_runner("ORELSE (FALSE, TRUE)", test_formula, expected)
  
  let test12 =
  	let test_formula = ORELSE (TRUE, FALSE) in
  	let expected = true in
  	test_runner("ORELSE (TRUE, FALSE)", test_formula, expected)
  
  let test13 =
  	let test_formula = ORELSE (FALSE, FALSE) in
  	let expected = false in
  	test_runner("ORELSE (FALSE, FALSE)", test_formula, expected)
  
  let test14 =
  	let test_formula = IMPLY (TRUE, TRUE) in
  	let expected = true in
  	test_runner("IMPLY (TRUE, TRUE)", test_formula, expected)
  
  let test15 =
  	let test_formula = IMPLY (TRUE, FALSE) in
  	let expected = false in
  	test_runner("IMPLY (TRUE, FALSE)", test_formula, expected)
  
  let test16 =
  	let test_formula = IMPLY (FALSE, TRUE) in
  	let expected = true in
  	test_runner("IMPLY (FALSE, TRUE)", test_formula, expected)
  
  let test17 =
  	let test_formula = IMPLY (FALSE, FALSE) in
  	let expected = true in
  	test_runner("IMPLY (FALSE, FALSE)", test_formula, expected)
  
  let test18 =
  	let test_formula = IMPLY (ANDALSO(TRUE, FALSE), NOT (NOT TRUE)) in
  	let expected = true in
  	test_runner("IMPLY (ANDALSO(TRUE, FALSE), NOT (NOT TRUE))", test_formula, expected)
  
  let test19 =
  	let test_formula = IMPLY (ORELSE(TRUE, FALSE), NOT (NOT FALSE)) in
  	let expected = false in
  	test_runner("IMPLY (ORELSE(TRUE, FALSE), NOT (NOT FALSE))", test_formula, expected)
  
  let test20 =
  	let test_formula = LESS((NUM 5), (NUM 6)) in
  	let expected = true in
  	test_runner("LESS((NUM 5), (NUM 6))", test_formula, expected)
  
  let test21 =
  	let test_formula = LESS((NUM 6), (NUM 6)) in
  	let expected = false in
  	test_runner("LESS((NUM 6), (NUM 6))", test_formula, expected)
  
  let test22 =
  	let test_formula = LESS((NUM 7), (NUM 6)) in
  	let expected = false in
  	test_runner("LESS((NUM 7), (NUM 6))", test_formula, expected)
  
  let test23 =
  	let test_formula = LESS(PLUS((NUM 5), (NUM 6)), (NUM 10)) in
  	let expected = false in
  	test_runner("LESS(PLUS((NUM 5), (NUM 6)), (NUM 10))", test_formula, expected)
  
  let test24 =
  	let test_formula = LESS(PLUS((NUM 5), (NUM 6)), (NUM 11)) in
  	let expected = false in
  	test_runner("LESS(PLUS((NUM 5), (NUM 6)), (NUM 11))", test_formula, expected)
  
  let test25 =
  	let test_formula = LESS(PLUS((NUM 5), (NUM 6)), (NUM 12)) in
  	let expected = true in
  	test_runner("LESS(PLUS((NUM 5), (NUM 6)), (NUM 12))", test_formula, expected)
  /* end of test code */
  */
}