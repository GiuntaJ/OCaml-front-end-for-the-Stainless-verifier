import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub231 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def _checkMetro(((m, nList))) = {
        m match {
          case STATION(n1) => { nList.contains(n1) }
          case AREA(n1, m1) => { _checkMetro(m1, List(n1) ++ nList) }
          case CONNECT(m1, m2) => {
            _checkMetro(m1, nList) && _checkMetro(m2, nList)
          }
        }
      }
      _checkMetro(metro, Nil())
    }
  }
          
}