import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub348 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkInclude(m, l) = {
        m match {
          case STATION(s) => { l.contains(s) }
          case AREA(a, inner_m) => { checkInclude(inner_m, a :: l) }
          case CONNECT(a, b) => { checkInclude(a, l) && checkInclude(b, l) }
        }
      }
      checkInclude(met, Nil())
    }
  }
}