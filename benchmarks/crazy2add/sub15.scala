import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub15 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed case class Error(param0: String) extends Exception {}
  
  def crazy2add(((ain, bin))) = {
    val _2 = {
      def filter(x) = {
        x match {
          case NIL => { x }
          case ZERO(y) => { if (filter(y) == ZERO(NIL)) ZERO(NIL) else x }
          case ONE(y) => { ONE(filter(y)) }
          case MONE(y) => { MONE(filter(y)) }
        }
      }
      val _3 = {
        def adder(((a, b))) = {
          a match {
            case ZERO(x) => {
              b match {
                case ONE(y) => { ONE(adder(x, y)) }
                case MONE(y) => { MONE(adder(x, y)) }
                case ZERO(y) => { ZERO(adder(x, y)) }
                case _ => { a }
              }
            }
            case ONE(x) => {
              b match {
                case ONE(y) => { ZERO(adder(ONE(NIL), adder(x, y))) }
                case MONE(y) => { ZERO(adder(x, y)) }
                case ZERO(y) => { ONE(adder(x, y)) }
                case _ => { a }
              }
            }
            case MONE(x) => {
              b match {
                case ONE(y) => { ZERO(adder(x, y)) }
                case MONE(y) => { ZERO(adder(MONE(NIL), adder(x, y))) }
                case ZERO(y) => { MONE(adder(x, y)) }
                case _ => { a }
              }
            }
            case _ => { b }
          }
        }
        filter(adder(ain, bin))
      }
    }
  }
}