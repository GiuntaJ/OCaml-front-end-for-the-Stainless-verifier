import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub488 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkMetro_0(met: Metro, lst: List[String]) = {
        met match {
          case STATION(n) => { lst.exists(( (x) => { x == n } )) }
          case AREA(n, met) => { checkMetro_0(met, n :: lst) }
          case CONNECT(met1, met2) => {
            checkMetro_0(met1, lst) && checkMetro_0(met2, lst)
          }
        }
      }
      checkMetro_0(met, Nil())
    }
  }
}