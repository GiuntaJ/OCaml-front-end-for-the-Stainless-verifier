import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub246 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def check_aux(l: List[Name], m: Metro): Boolean = {
        m match {
          case STATION(n) => { l.contains(n) }
          case AREA(n1, m1) => {
            if (l.contains(n1)) check_aux(l, m1) else check_aux(n1 :: l, m1)
          }
          case CONNECT(m1, m2) => { check_aux(l, m1) && check_aux(l, m2) }
        }
      }
      check_aux(Nil(), met)
    }
  }
}