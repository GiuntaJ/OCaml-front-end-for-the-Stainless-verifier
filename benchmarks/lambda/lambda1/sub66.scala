import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub66 {
  
  /* ex 7 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkList(((s, l))) = {
        l match {
          case Nil() => { false }
          case Cons(h, t) => { if (s == h) true else checkList(s, t) }
        }
      }
      val _3 = {
        def cal(((m, l))) = {
          m match {
            case STATION(n) => { checkList(n, l) }
            case AREA(n, m1) => { cal(m1, n :: l) }
            case CONNECT(m1, m2) => { cal(m1, l) && cal(m2, l) }
          }
        }
        cal(met, Nil())
      }
    }
  }
  
}
