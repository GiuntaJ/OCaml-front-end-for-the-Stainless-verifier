import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub454 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def check: (Metro, List[Name]) => Boolean = {
            case (m, nl) =>
              {
                m match {
                  case STATION(n) => { nl.exists(( (_n) => { _n == n } )) }
                  case AREA(n, _m) => { check(_m, n :: nl) }
                  case CONNECT(m1, m2) => { check(m1, nl) && check(m2, nl) }
                }
            }
          }
          check(m, Nil())
        }
    }
  )
}