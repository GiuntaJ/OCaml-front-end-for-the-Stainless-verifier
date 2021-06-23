import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub461 {
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def findname: (Metro, List[Name]) => Boolean = {
            case (n, l) =>
              {
                n match {
                  case STATION(o) => { l.contains(o) }
                  case AREA(o, p) => { findname(p, o :: l) }
                  case CONNECT(p, q) => { findname(p, l) && findname(q, l) }
                }
            }
          }
          findname(m, Nil())
        }
    }
  )
}