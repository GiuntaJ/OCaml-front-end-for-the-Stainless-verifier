import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub419 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkArea(((m: Metro, names: List[Name]))): Boolean = {
        m match {
          case STATION(n) => { names.contains(n) }
          case AREA(n, m1) => { checkArea(m1, names ++ List(n)) }
          case CONNECT(m1, m2) => { checkArea(m1, names) && checkArea(m2, names)
          }
        }
      }
      checkArea(m, Nil())
    }
  }
}
