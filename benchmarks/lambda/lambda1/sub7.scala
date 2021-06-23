import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub7 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkArea(id, m) = {
        m match {
          case STATION(n) => { id.exists(( (x) => { x == n } )) }
          case CONNECT(s1, s2) => { checkArea(id, s1) && checkArea(id, s2) }
          case AREA(id2, m) => { checkArea(id2 :: id, m) }
        }
      }
      met match {
        case AREA(id, m) => { checkArea(List(id), m) }
        case _ => { false }
      }
    }
  }
}