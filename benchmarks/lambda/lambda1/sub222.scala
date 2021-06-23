import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub222 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkName(((str, l))) = {
        l match {
          case Nil() => { false }
          case Cons(head, tail) => {
            if (str == head) true else checkName(str, tail)
          }
        }
      }
      val _3 = {
        def checkMetro2(((m, l))) = {
          m match {
            case STATION(a) => { checkName(a, l) }
            case AREA(a, b) => { checkMetro2(b, a :: l) }
            case CONNECT(a, b) => { checkMetro2(a, l) && checkMetro2(b, l) }
          }
        }
        checkMetro2(m, Nil())
      }
    }
  }
}