import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub206 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  /*
  let c2 : crazy2 = ZERO(ONE(MONE NIL)) /*-2*/
  let c3 : crazy2 = MONE(ONE(ONE(ZERO(MONE NIL)))) /*-11*/
  let _ = if crazy2val c2 == -2 then print_endline ("O")
  		  else print_endline ("X")
  let _ = if crazy2val c3 == -11 then print_endline ("O")
  		  else print_endline ("X")
  */
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    c1 match {
      case NIL => { c2 }
      case ZERO(c1_t) => {
        c2 match {
          case NIL => { c1 }
          case ZERO(c2_t) => { ZERO(crazy2add(c1_t, c2_t)) }
          case ONE(c2_t) => { ONE(crazy2add(c1_t, c2_t)) }
          case MONE(c2_t) => { MONE(crazy2add(c1_t, c2_t)) }
        }
      }
      case ONE(c1_t) => {
        c2 match {
          case NIL => { c1 }
          case ZERO(c2_t) => { ONE(crazy2add(c1_t, c2_t)) }
          case ONE(c2_t) => { ZERO(crazy2add(ONE(NIL), crazy2add(c1_t, c2_t))) }
          case MONE(c2_t) => { ZERO(crazy2add(c1_t, c2_t)) }
        }
      }
      case MONE(c1_t) => {
        c2 match {
          case NIL => { c1 }
          case ZERO(c2_t) => { MONE(crazy2add(c1_t, c2_t)) }
          case ONE(c2_t) => { ZERO(crazy2add(c1_t, c2_t)) }
          case MONE(c2_t) => { ZERO(crazy2add(MONE(NIL), crazy2add(c1_t, c2_t)))
          }
        }
      }
    }
  }
  
  
  /*
  let c2 : crazy2 = ZERO(ONE(MONE NIL)) /*-2*/
  let c3 : crazy2 = MONE(ONE(ONE(ZERO(MONE NIL)))) /*-11*/
  let c4 : crazy2 = crazy2add (c2,c3) /*-13*/
  
  let _ = if crazy2val c4 == -13 then print_endline ("O")
  		  else print_endline ("X")
  */
}
