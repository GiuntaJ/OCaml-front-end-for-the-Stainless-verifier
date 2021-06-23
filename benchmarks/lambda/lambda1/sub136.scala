import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub136 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def chk(((x, y))) = {
        x match {
          case STATION(a) => { y.contains(a) }
          case AREA(a, b) => { chk(b, a :: y) }
          case CONNECT(a, b) => { chk(a, y) && chk(b, y) }
        }
      }
      chk(met, Nil())
    }
  }
}
