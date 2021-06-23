import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub478 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def evalMetro(((areas: List[Name], m_eval: Metro))) = {
        m_eval match {
          case STATION(sname) => { areas.contains(sname) }
          case AREA(aname, m_prime) => { evalMetro(aname :: areas, m_prime) }
          case CONNECT(m1, m2) => { evalMetro(areas, m1) && evalMetro(areas, m2)
          }
        }
      }
      evalMetro(Nil(), m)
    }
  }
}