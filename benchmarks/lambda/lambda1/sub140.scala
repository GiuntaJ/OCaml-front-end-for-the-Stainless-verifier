import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub140 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def sub_check: (Metro, List[String]) => Boolean = {
            case (m, l) =>
              {
                m match {
                  case STATION(n) => { l.exists(( (x) => { x == n } )) }
                  case AREA(n, m) => { sub_check(m, n :: l) }
                  case CONNECT(m1, m2) => { sub_check(m1, l) && sub_check(m2, l)
                  }
                }
            }
          }
          sub_check(m, Nil())
        }
    }
  )
}
