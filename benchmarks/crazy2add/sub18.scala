import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub18 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  sealed case class E() extends Exception {}
  
  def crazy2add(((a, b))) = {
    val _2 = {
      def add(((a, b))) = {
        (a, b) match {
          case (ZERO(c), ZERO(d)) => { ZERO(crazy2add(c, d)) }
          case (ONE(c), MONE(d)) => { ZERO(crazy2add(c, d)) }
          case (MONE(c), ONE(d)) => { ZERO(crazy2add(c, d)) }
          case (ZERO(c), ONE(d)) => { ONE(crazy2add(c, d)) }
          case (ONE(c), ZERO(d)) => { ONE(crazy2add(c, d)) }
          case (ZERO(c), MONE(d)) => { MONE(crazy2add(c, d)) }
          case (MONE(c), ZERO(d)) => { MONE(crazy2add(c, d)) }
          case (ONE(c), ONE(d)) => { ZERO(crazy2add(ONE(NIL), crazy2add(c, d)))
          }
          case (MONE(c), MONE(d)) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(c, d)))
          }
          case (NIL, b) => { b }
          case (a, NIL) => { a }
        }
      }
      val _3 = {
        def trim(a) = {
          a match {
            case NIL => { NIL }
            case ONE(b) => { ONE(trim(b)) }
            case MONE(b) => { MONE(trim(b)) }
            case ZERO(b) => {
              b match {
                case NIL => { NIL }
                case ONE(c) => { ZERO(trim(b)) }
                case MONE(c) => { ZERO(trim(b)) }
                case ZERO(c) => { trim(ZERO(trim(b))) }
              }
            }
          }
        }
        val _4 = {
          def zero(a) = {
            a match {
              case NIL => { ZERO(NIL) }
              case _ => { a }
            }
          }
          zero(trim(add(a, b)))
        }
      }
    }
  }
}