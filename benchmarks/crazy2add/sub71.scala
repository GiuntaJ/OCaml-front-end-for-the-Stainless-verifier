import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub71 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((num1, num2))) = {
    num1 match {
      case NIL => { num2 }
      case ZERO(num1_0) => {
        num2 match {
          case NIL => { num1 }
          case ZERO(num2_0) => { ZERO(crazy2add(num1_0, num2_0)) }
          case ONE(num2_0) => { ONE(crazy2add(num1_0, num2_0)) }
          case MONE(num2_0) => { MONE(crazy2add(num1_0, num2_0)) }
        }
      }
      case ONE(num1_0) => {
        num2 match {
          case NIL => { num1 }
          case ZERO(num2_0) => { ONE(crazy2add(num1_0, num2_0)) }
          case ONE(num2_0) => {
            ZERO(crazy2add(ONE(NIL), crazy2add(num1_0, num2_0)))
          }
          case MONE(num2_0) => { ZERO(crazy2add(num1_0, num2_0)) }
        }
      }
      case MONE(num1_0) => {
        num2 match {
          case NIL => { num1 }
          case ZERO(num2_0) => { MONE(crazy2add(num1_0, num2_0)) }
          case ONE(num2_0) => { ZERO(crazy2add(num1_0, num2_0)) }
          case MONE(num2_0) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(num1_0, num2_0)))
          }
        }
      }
    }
  }
}