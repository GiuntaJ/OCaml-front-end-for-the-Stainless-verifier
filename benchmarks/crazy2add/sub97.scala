import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub97 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (x, y) =>
      {
        (x, y) match {
          case (NIL, _) => { y }
          case (_, NIL) => { x }
          case (ZERO(c1), ZERO(c2)) => { ZERO(crazy2add(c1, c2)) }
          case (ZERO(c1), ONE(c2)) | (ONE(c1), ZERO(c2)) => {
            ONE(crazy2add(c1, c2))
          }
          case (ZERO(c1), MONE(c2)) | (MONE(c1), ZERO(c2)) => {
            MONE(crazy2add(c1, c2))
          }
          case (ONE(c1), MONE(c2)) | (MONE(c1), ONE(c2)) => {
            ZERO(crazy2add(c1, c2))
          }
          case (ONE(c1), ONE(c2)) => {
            val _5 = {
              val carry = ONE(NIL)
              ZERO(crazy2add(carry, crazy2add(c1, c2)))
            }
          }
          case (MONE(c1), MONE(c2)) => {
            val _2 = {
              val carry = MONE(NIL)
              ZERO(crazy2add(carry, crazy2add(c1, c2)))
            }
          }
        }
    }
  }
}