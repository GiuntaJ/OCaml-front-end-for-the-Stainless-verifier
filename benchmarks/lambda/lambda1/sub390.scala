import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub390 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (x) =>
      {
        val _4 = {
          def checkMetro_temp(x, l: List[Name]): Boolean = {
            x match {
              case STATION(n) => { l.exists(( (x) => { x == n } )) }
              case AREA(n, m) => { checkMetro_temp(m, n :: l) }
              case CONNECT(m1, m2) => {
                if (checkMetro_temp(m1, l)) checkMetro_temp(m2, l) else false
              }
            }
          }
          checkMetro_temp(x, Nil())
        }
    }
  )
}