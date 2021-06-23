import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub286 {
  sealed abstract class Metro {}
  case class STATION(param0: String) extends Metro {}
  case class AREA(param0: String,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def chk(st, at) = {
        at match {
          case Cons(hd, tl) => { if (hd == st) true else chk(st, tl) }
          case Nil() => { false }
        }
      }
      val _3 = {
        def foo(mtr, lst) = {
          mtr match {
            case STATION(s) => { chk(s, lst) }
            case AREA(a, rest) => { foo(rest, a :: lst) }
            case CONNECT(a, b) => { foo(a, lst) && foo(b, lst) }
          }
        }
        foo(metro, Nil())
      }
    }
  }
}
