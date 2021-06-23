import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub130 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def check(m, set) = {
        m match {
          case STATION(n) => { set.contains(n) }
          case AREA(n, m) => { check(m, n :: set) }
          case CONNECT(m1, m2) => { check(m1, set) && check(m2, set) }
        }
      }
      check(m, Nil())
    }
  }
}