import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub120 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkArea(((m, nl))) = {
        m match {
          case STATION(n) => { nl.contains(n) }
          case AREA(n, m) => { checkArea(m, n :: nl) }
          case CONNECT(m1, m2) => { checkArea(m1, nl) && checkArea(m2, nl) }
        }
      }
      checkArea(m, Nil())
    }
  }
}