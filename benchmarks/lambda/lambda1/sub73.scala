import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub73 {
  /* 2009-11824 Jieun-Jeong HW1-7 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(input: Metro): Boolean = {
    val _2 = {
      def is_in_area(n, lst) = {
        lst match {
          case Nil() => { false }
          case Cons(a, l) => { if (a == n) true else is_in_area(n, l) }
        }
      }
      val _3 = {
        def check(m, lst) = {
          m match {
            case STATION(n) => { is_in_area(n, lst) }
            case AREA(n, x) => { check(x, n :: lst) }
            case CONNECT(l, r) => { if (check(l, lst)) check(r, lst) else false
            }
          }
        }
        check(input, Nil())
      }
    }
  }
}
