import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub212 {
  
  /* EXERCISE 2,3 */
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (in1, in2) =>
      {
        in1 match {
          case NIL => { in2 }
          case ZERO(subin1) => {
            in2 match {
              case NIL => { ZERO(subin1) }
              case ZERO(subin2) => { ZERO(crazy2add(subin1, subin2)) }
              case ONE(subin2) => { ONE(crazy2add(subin1, subin2)) }
              case MONE(subin2) => { MONE(crazy2add(subin1, subin2)) }
            }
          }
          case ONE(subin1) => {
            in2 match {
              case NIL => { ONE(subin1) }
              case ZERO(subin2) => { ONE(crazy2add(subin1, subin2)) }
              case ONE(subin2) => {
                ZERO(crazy2add(ONE(NIL), crazy2add(subin1, subin2)))
              }
              case MONE(subin2) => { ZERO(crazy2add(subin1, subin2)) }
            }
          }
          case MONE(subin1) => {
            in2 match {
              case NIL => { MONE(subin1) }
              case ZERO(subin2) => { MONE(crazy2add(subin1, subin2)) }
              case ONE(subin2) => { ZERO(crazy2add(subin1, subin2)) }
              case MONE(subin2) => {
                ZERO(crazy2add(MONE(NIL), crazy2add(subin1, subin2)))
              }
            }
          }
        }
    }
  }
  
  /*
  let _ = 
  let print_bool x = 
  print_endline (string_of_bool x) in 
  print_bool(0 == crazy2val (ZERO(NIL)));
  print_bool(7 == crazy2val (ONE(ONE(ONE(NIL)))));
  print_bool(-7 == crazy2val (MONE(MONE(MONE(NIL)))));
  print_bool(10 == crazy2val (ZERO(ONE(ZERO(ONE(NIL))))));
  print_bool(-10 == crazy2val (ZERO(MONE(ZERO(MONE(NIL))))));
  print_bool(0 == crazy2val (ZERO(ZERO(ZERO(ZERO(NIL))))));
  print_bool(-5 == crazy2val (ONE(ONE(ZERO(MONE(NIL))))));
  print_bool(-6 == crazy2val(crazy2add (ZERO(NIL), ZERO(MONE(MONE(NIL))))));
  print_bool(22 == crazy2val(crazy2add (ONE(ONE(ONE(NIL))),ONE(ONE(ONE(ONE(NIL)))))));
  print_bool(-22 == crazy2val(crazy2add (MONE(MONE(MONE(NIL))),MONE(MONE(MONE(MONE(NIL)))))));
  print_bool(4 == crazy2val(crazy2add (ZERO(ONE(ZERO(ONE(MONE(NIL))))), ZERO(ONE(ZERO(ONE(NIL)))))));
  print_bool(-4 == crazy2val(crazy2add (ZERO(MONE(ZERO(MONE(ONE(NIL))))), ZERO(MONE(ZERO(MONE(NIL)))))));
  print_bool(0 == crazy2val(crazy2add (ZERO(ZERO(ZERO(ZERO(ZERO(NIL))))), ZERO(ZERO(ZERO(ZERO(NIL)))))));
  print_bool(-10 == crazy2val(crazy2add (ONE(ONE(ZERO(MONE(NIL)))), ONE(ONE(ZERO(MONE(NIL)))))));
  */
}