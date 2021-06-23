import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub29 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def check(met, lst) = {
        met match {
          case STATION(id) => { lst.contains(id) }
          case AREA(id, m) => { check(m, lst ++ List(id)) }
          case CONNECT(m1, m2) => { check(m1, lst) && check(m2, lst) }
        }
      }
      met match {
        case STATION(id) => { false }
        case AREA(id, m) => { check(m, List(id)) }
        case CONNECT(m1, m2) => { false }
      }
    }
  }
  
}
