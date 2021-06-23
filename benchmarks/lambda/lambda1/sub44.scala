import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub44 {
  /* complete */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def search(n, l) = {
        l match {
          case Nil() => { false }
          case Cons(h, t) => { if (h == n) true else search(n, t) }
        }
      }
      val _3 = {
        def check(m, lst) = {
          m match {
            case STATION(n) => { search(n, lst) }
            case AREA(a, b) => { check(b, a :: lst) }
            case CONNECT(a, b) => { check(a, lst) && check(b, lst) }
          }
        }
        check(met, Nil())
      }
    }
  }
}