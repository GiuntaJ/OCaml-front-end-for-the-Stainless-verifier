import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub89 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def check(m, a) = {
        m match {
          case STATION(n) => { a.contains(n) }
          case AREA(n, m1) => { check(m1, n :: a) }
          case CONNECT(m1, m2) => { check(m1, a) && check(m2, a) }
        }
      }
      check(m, Nil())
    }
  }
}