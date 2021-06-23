import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub4 {
  sealed case class Error(param0: String) extends Exception {}
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a, b))) = {
    val _2 = {
      def crazy2min(c) = {
        val _5 = {
          def crazy2min_sub(c) = {
            val _8 = {
              val n = crazy2min(c)
              if (n == ZERO(NIL)) NIL else n
            }
          }
          c match {
            case NIL => { NIL }
            case ZERO(NIL) => { ZERO(NIL) }
            case ZERO(c) => { ZERO(crazy2min_sub(c)) }
            case ONE(c) => { ONE(crazy2min_sub(c)) }
            case MONE(c) => { MONE(crazy2min_sub(c)) }
          }
        }
      }
      val _9 = {
        def crazy2add_sub(((a, b))) = {
          (a, b) match {
            case (NIL, c) | (c, NIL) => { c }
            case (ZERO(c1), ZERO(c2)) | (ONE(c1), MONE(c2)) |
            (MONE(c1), ONE(c2)) => {
              ZERO(crazy2add_sub(c1, c2))
            }
            case (ZERO(c1), ONE(c2)) | (ONE(c1), ZERO(c2)) => {
              ONE(crazy2add_sub(c1, c2))
            }
            case (ZERO(c1), MONE(c2)) | (MONE(c1), ZERO(c2)) => {
              MONE(crazy2add_sub(c1, c2))
            }
            case (ONE(c1), ONE(c2)) => {
              ZERO(crazy2add_sub(ONE(NIL), crazy2add_sub(c1, c2)))
            }
            case (MONE(c1), MONE(c2)) => {
              ZERO(crazy2add_sub(MONE(NIL), crazy2add_sub(c1, c2)))
            }
          }
        }
        (a, b) match {
          case (NIL, c) | (c, NIL) => { assert(false, "Error with Exception") }
          case _ => { crazy2min(crazy2add_sub(a, b)) }
        }
      }
    }
  }
}