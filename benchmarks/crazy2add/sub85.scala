import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub85 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed case class InconsistencyError() extends Exception {}
  
  def crazy2add(((v1: Crazy2, v2: Crazy2))) = {
    val _2 = {
      def digit_identifier(v: Crazy2): (Int63, Crazy2) = {
        v match {
          case NIL => { (0, NIL) }
          case ZERO(c) => { (0, c) }
          case ONE(c) => { (1, c) }
          case MONE(c) => { (-(1), c) }
        }
      }
      val _3 = {
        def add(((v1: Crazy2, v2: Crazy2, carry: Int63))): Crazy2 = {
          (v1, v2, carry) match {
            case (NIL, NIL, 0) => { NIL }
            case (NIL, NIL, v) => {
              
                if (
                  v == 1
                ) {
                  ONE(NIL) 
                } else if (
                  v == 0
                ) {
                  ZERO(NIL) 
                } else {
                  MONE(NIL)
                }
            }
            case (_, _, _) => {
              val _6 = {
                val ((d1, c1)) = digit_identifier(v1)
                val _7 = {
                  val ((d2, c2)) = digit_identifier(v2)
                  val _8 = {
                    val s = d1 + d2 + carry
                    s match {
                      case -2 => { ZERO(add(c1, c2, -(1))) }
                      case -1 => { MONE(add(c1, c2, 0)) }
                      case 0 => { ZERO(add(c1, c2, 0)) }
                      case 1 => { ONE(add(c1, c2, 0)) }
                      case 2 => { ZERO(add(c1, c2, 1)) }
                      case _ => { assert(false, "InconsistencyError") }
                    }
                  }
                }
              }
            }
          }
        }
        add(v1, v2, 0)
      }
    }
  }
}