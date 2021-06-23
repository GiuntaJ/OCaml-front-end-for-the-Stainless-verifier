import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub127 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (x) =>
      {
        val _4 = {
          def icm(x, l) = {
            x match {
              case STATION(n) => { l.contains(n) }
              case CONNECT(a, b) => { icm(a, l) && icm(b, l) }
              case AREA(n, a) => { icm(a, l ++ List(n)) }
            }
          }
          icm(x, Nil())
        }
    }
  )
}