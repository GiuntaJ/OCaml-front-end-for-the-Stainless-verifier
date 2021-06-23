import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub225 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def cm_iter(met: Metro, anl: List[Name]): Boolean = {
        met match {
          case STATION(n) => { anl.contains(n) }
          case AREA(p, q) => { cm_iter(q, p :: anl) }
          case CONNECT(p, q) => { cm_iter(p, anl) && cm_iter(q, anl) }
        }
      }
      cm_iter(m, Nil())
    }
  }
}