import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub125 {
  sealed abstract class Metro {}
  case class STATION(param0: String) extends Metro {}
  case class AREA(param0: String,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def chk(what, at) = {
        at match {
          case Cons(hd, tl) => { if (hd == what) true else chk(what, tl) }
          case Nil() => { false }
        }
      }
      val _3 = {
        def foo(mtr, lst) = {
          mtr match {
            case STATION(n) => { chk(n, lst) }
            case AREA(n, rest) => { foo(rest, n :: lst) }
            case CONNECT(a, b) => { foo(a, lst) && foo(b, lst) }
          }
        }
        foo(metro, Nil())
      }
    }
  }
}
