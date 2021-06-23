import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub157 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def return0(met, lst) = {
        val _5 = {
          def find(a, lst) = {
            lst match {
              case Nil() => { false }
              case Cons(head, tail) => { if (head == a) true else find(a, tail)
              }
            }
          }
          met match {
            case STATION(a) => { find(a, lst) }
            case AREA(a, b) => { return0(b, a :: lst) }
            case CONNECT(a, b) => { return0(a, lst) && return0(b, lst) }
          }
        }
      }
      return0(met, Nil())
    }
  }	
}