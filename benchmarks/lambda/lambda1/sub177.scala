import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub177 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def check(m) = {
        m match {
          case STATION(x) => { List(x) }
          case AREA(x, y) => { check(y).filter(( (n) => { not(x == n) } )) }
          case CONNECT(x, y) => { check(x) ++ check(y) }
        }
      }
      check(metro).length == 0
    }
  }
}