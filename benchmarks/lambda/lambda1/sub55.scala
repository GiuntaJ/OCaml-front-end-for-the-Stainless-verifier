import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub55 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def check(met, ids) = {
        met match {
          case STATION(id) => { ids.contains(id) }
          case AREA(id, m) => { check(m, id :: ids) }
          case CONNECT(m, m_0) => { check(m, ids) && check(m_0, ids) }
        }
      }
      check(metro, Nil())
    }
  }
}