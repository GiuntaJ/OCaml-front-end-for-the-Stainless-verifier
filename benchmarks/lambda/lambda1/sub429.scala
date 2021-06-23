import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub429 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def helper(((met: Metro, l: List[Name]))): Boolean = {
        met match {
          case STATION(x) => { l.contains(x) }
          case AREA(x, y) => { helper(y, x :: l) }
          case CONNECT(y, z) => { helper(y, l) && helper(z, l) }
        }
      }
      helper(m, Nil())
    }
  }
}