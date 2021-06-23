import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub145 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(a: Metro): Boolean = {
    val _2 = {
      def contains(x, lst) = {
        lst match {
          case Nil() => { false }
          case Cons(hd, rest) => { if (hd == x) true else contains(x, rest) }
        }
      }
      val _3 = {
        def realcheckMetro(a, lst) = {
          a match {
            case STATION(x) => { contains(x, lst) }
            case AREA(x, y) => { realcheckMetro(y, x :: lst) }
            case CONNECT(x, y) => {
              realcheckMetro(x, lst) && realcheckMetro(y, lst)
            }
          }
        }
        realcheckMetro(a, Nil())
      }
    }
  }
  
  	
}