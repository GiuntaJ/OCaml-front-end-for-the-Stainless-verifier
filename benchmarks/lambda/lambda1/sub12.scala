import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub12 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(a: Metro): Boolean = {
    val _2 = {
      def checkm(m, ls) = {
        m match {
          case STATION(name) => { ls.contains(name) }
          case AREA(n, m) => { checkm(m, n :: ls) }
          case CONNECT(m1, m2) => { checkm(m1, ls) && checkm(m2, ls) }
        }
      }
      checkm(a, Nil())
    }
  }
}