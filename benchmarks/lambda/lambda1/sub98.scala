import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub98 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def ismem(m, lst) = {
        m match {
          case STATION(n) => { lst.contains(n) }
          case AREA(n, m1) => { ismem(m1, n :: lst) }
          case CONNECT(m1, m2) => { ismem(m1, lst) && ismem(m2, lst) }
        }
      }
      ismem(m, Nil())
    }
  }
}