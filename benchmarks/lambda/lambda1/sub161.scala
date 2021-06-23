import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub161 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def aux(m, ids) = {
        m match {
          case STATION(id) => { ids.contains(id) }
          case AREA(id, m_0) => { aux(m_0, id :: ids) }
          case CONNECT(m1, m2) => { aux(m1, ids) && aux(m2, ids) }
        }
      }
      aux(met, Nil())
    }
  }
}