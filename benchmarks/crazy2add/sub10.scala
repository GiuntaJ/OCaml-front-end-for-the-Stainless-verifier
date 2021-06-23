import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub10 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a, b))) = {
    val _2 = {
      def adder(((a, b))) = {
        a match {
          case ZERO(aa) => {
            b match {
              case ZERO(bb) => { ZERO(adder(aa, bb)) }
              case ONE(bb) => { ONE(adder(aa, bb)) }
              case MONE(bb) => { MONE(adder(aa, bb)) }
              case NIL => { ZERO(aa) }
            }
          }
          case ONE(aa) => {
            b match {
              case ZERO(bb) => { ONE(adder(aa, bb)) }
              case ONE(bb) => { ZERO(adder(adder(aa, ONE(NIL)), bb)) }
              case MONE(bb) => { ZERO(adder(aa, bb)) }
              case NIL => { ONE(aa) }
            }
          }
          case MONE(aa) => {
            b match {
              case ZERO(bb) => { MONE(adder(aa, bb)) }
              case ONE(bb) => { ZERO(adder(aa, bb)) }
              case MONE(bb) => { ZERO(adder(adder(aa, ONE(NIL)), bb)) }
              case NIL => { MONE(aa) }
            }
          }
          case NIL => {
            b match {
              case ZERO(bb) => { ZERO(bb) }
              case ONE(bb) => { ONE(bb) }
              case MONE(bb) => { MONE(bb) }
              case NIL => { NIL }
            }
          }
        }
      }
      val _3 = {
        def rid(a) = {
          a match {
            case NIL => { NIL }
            case ONE(b) => { ONE(rid(b)) }
            case MONE(b) => { MONE(rid(b)) }
            case ZERO(b) => {
              b match {
                case NIL => { NIL }
                case ONE(aa) => { ZERO(rid(b)) }
                case MONE(aa) => { ZERO(rid(b)) }
                case ZERO(aa) => { rid(ZERO(rid(b))) }
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
          zero(rid(adder(a, b)))
        }
      }
    }
  }
}