import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub162 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def subcheckMetro(m, areas) = {
        m match {
          case STATION(name) => { areas.contains(name) }
          case AREA(name, met1) => { subcheckMetro(met1, name :: areas) }
          case CONNECT(met1, met2) => {
            subcheckMetro(met1, areas) && subcheckMetro(met2, areas)
          }
        }
      }
      subcheckMetro(met, Nil())
    }
  }
  
}
