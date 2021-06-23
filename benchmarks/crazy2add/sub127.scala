import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub127 {
  /*2016-11690*/
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (cra1, cra2) =>
      {
        cra1 match {
          case NIL => { cra2 }
          case ZERO(num1) => {
            cra2 match {
              case NIL => { cra1 }
              case ZERO(num2) => { ZERO(crazy2add(num1, num2)) }
              case ONE(num2) => { ONE(crazy2add(num1, num2)) }
              case MONE(num2) => { MONE(crazy2add(num1, num2)) }
            }
          }
          case ONE(num1) => {
            cra2 match {
              case NIL => { cra1 }
              case ZERO(num2) => { ONE(crazy2add(num1, num2)) }
              case ONE(num2) => {
                ZERO(crazy2add(ONE(NIL), crazy2add(num1, num2)))
              }
              case MONE(num2) => { ZERO(crazy2add(num1, num2)) }
            }
          }
          case MONE(num1) => {
            cra2 match {
              case NIL => { cra1 }
              case ZERO(num2) => { MONE(crazy2add(num1, num2)) }
              case ONE(num2) => { ZERO(crazy2add(num1, num2)) }
              case MONE(num2) => {
                ZERO(crazy2add(MONE(NIL), crazy2add(num1, num2)))
              }
            }
          }
        }
    }
  }
  
  
  /* test code 
  
  
  
  
  let rec crazy2val : crazy2 -> int = fun num ->
  	match num with
  	| NIL -> 0
  	| ZERO um -> 2 * (crazy2val um)
  	| ONE um -> 2 * (crazy2val um) + 1
  	| MONE um -> 2 * (crazy2val um) - 1
  
  let _= 
  let print_bool x = print_endline (string_of_bool x) in 
  
  print_bool (0 = (crazy2val (crazy2add (ZERO NIL, ZERO NIL)))); 
  print_bool (0 = (crazy2val (crazy2add (MONE NIL, ONE NIL)))); 
  print_bool (1 = (crazy2val (crazy2add (ZERO NIL, ONE NIL)))); 
  print_bool (4 = (crazy2val (crazy2add (ONE (ONE NIL), ONE NIL)))); 
  print_bool (-683 = (crazy2val (crazy2add (MONE (ZERO (ZERO (ZERO NIL))), (ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL))))))))))))))) 
  ;; 
  let rec crazy2val : crazy2 -> int = fun num ->
  	match num with
  	| NIL -> 0
  	| ZERO um -> 2 * (crazy2val um)
  	| ONE um -> 2 * (crazy2val um) + 1
  	| MONE um -> 2 * (crazy2val um) - 1
  
  */
}