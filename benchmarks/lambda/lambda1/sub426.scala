import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub426 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def check_help: (Metro, List[Name]) => Boolean = {
            case (m, areas) =>
              {
                m match {
                  case STATION(v) => { areas.contains(v) }
                  case AREA(a, b) => { check_help(b, a :: areas) }
                  case CONNECT(a, b) => {
                    check_help(a, areas) && check_help(b, areas)
                  }
                }
            }
          }
          check_help(m, Nil())
        }
    }
  )
}