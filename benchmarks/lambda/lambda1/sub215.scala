import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub215 {
  /* hw2-1 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetroHelper(g, m) = {
        m match {
          case STATION(s) => { g.contains(s) }
          case AREA(a, n) => { checkMetroHelper(a :: g, n) }
          case CONNECT(n1, n2) => {
            checkMetroHelper(g, n1) && checkMetroHelper(g, n2)
          }
        }
      }
      checkMetroHelper(Nil(), m)
    }
  }
}