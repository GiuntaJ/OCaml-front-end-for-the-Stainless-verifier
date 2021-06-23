import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub467 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetroRec(((m: Metro, a: List[String]))): Boolean = {
        m match {
          case STATION(n) => {
            if (a.exists(( (x) => { x == n } ))) true else false
          }
          case AREA(n, m2) => { checkMetroRec(m2, n :: a) }
          case CONNECT(m1, m2) => { checkMetroRec(m1, a) && checkMetroRec(m2, a)
          }
        }
      }
      checkMetroRec(m, Nil())
    }
  }
}