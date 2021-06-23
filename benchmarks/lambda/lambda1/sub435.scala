import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub435 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def check: (Metro, List[String]) => Boolean = {
            case (m, areas) =>
              {
                m match {
                  case STATION(a) => { areas.contains(a) }
                  case AREA(a, b) => { check(b, areas ++ List(a)) }
                  case CONNECT(a, b) => { check(a, areas) && check(b, areas) }
                }
            }
          }
          check(m, Nil())
        }
    }
  )
}
