import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub111 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def check(l, m) = {
        m match {
          case STATION(n) => { l.contains(n) }
          case AREA(n, m) => { check(n :: l, m) }
          case CONNECT(m1, m2) => { check(l, m1) && check(l, m2) }
        }
      }
      check(Nil(), m)
    }
  }
}