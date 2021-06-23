import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub228 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkMetrorec(met, lst): Boolean = {
        met match {
          case STATION(a) => { if (lst.contains(a) == true) true else false }
          case AREA(a, b) => { checkMetrorec(b, a :: lst) }
          case CONNECT(a, b) => { checkMetrorec(a, lst) && checkMetrorec(b, lst)
          }
        }
      }
      checkMetrorec(met, Nil())
    }
  }
}
