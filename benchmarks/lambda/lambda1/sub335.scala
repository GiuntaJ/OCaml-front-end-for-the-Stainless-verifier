import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub335 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def areaChecker(lst: List[String], m, b) = {
        m match {
          case STATION(n) => { b && lst.contains(n) }
          case AREA(n, m1) => { b && areaChecker(n :: lst, m1, b) }
          case CONNECT(m1, m2) => {
            b && areaChecker(lst, m1, b) && areaChecker(lst, m2, b)
          }
        }
      }
      areaChecker(Nil(), met, true)
    }
  }
}