import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub77 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checklst(((lst, x))) = {
        lst match {
          case Nil() => { false }
          case Cons(h, t) => { if (x == h) true else checklst(t, x) }
        }
      }
      val _3 = {
        def f(((m, lst))) = {
          m match {
            case STATION(x) => { checklst(lst, x) }
            case AREA(x, y) => { f(y, x :: lst) }
            case CONNECT(x, y) => { f(x, lst) && f(y, lst) }
          }
        }
        f(met, Nil())
      }
    }
  }
}
