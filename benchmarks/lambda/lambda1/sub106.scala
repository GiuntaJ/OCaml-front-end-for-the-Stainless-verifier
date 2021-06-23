import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub106 {
  /* hw1-8 */
  /* 2010-11687 Keunjun choi */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def check(((ms, l))) = {
        ms match {
          case STATION(a) => { l.contains(a) }
          case AREA(a, b) => { check(b, a :: l) }
          case CONNECT(a, b) => { check(a, l) && check(b, l) }
        }
      }
      check(m, Nil())
    }
  }
}