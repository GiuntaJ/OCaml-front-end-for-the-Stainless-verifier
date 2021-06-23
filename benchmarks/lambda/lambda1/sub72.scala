import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub72 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def cmhelper(m, l) = {
        m match {
          case STATION(a) => { l.exists(( (x) => { a == x } )) }
          case AREA(a, b) => { cmhelper(b, a :: l) }
          case CONNECT(a, b) => { cmhelper(a, l) && cmhelper(b, l) }
        }
      }
      cmhelper(m, Nil())
    }
  }
}