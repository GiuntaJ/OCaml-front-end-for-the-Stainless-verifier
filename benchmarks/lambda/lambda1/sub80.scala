import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub80 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def check(m, s) = {
        m match {
          case STATION(n) => { s.contains(n) }
          case AREA(n, m_0) => { check(m_0, n :: s) }
          case CONNECT(m1, m2) => { check(m1, s) && check(m2, s) }
        }
      }
      check(m, Nil())
    }
  }
}