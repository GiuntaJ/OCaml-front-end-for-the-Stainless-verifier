import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub159 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetroRec(m: Metro, s: List[Name]): Boolean = {
        m match {
          case STATION(n) => { s.contains(n) }
          case AREA(n, m) => {
            val _5 = {
              val ns: List[Name] = n :: s
              checkMetroRec(m, ns)
            }
          }
          case CONNECT(m1, m2) => { checkMetroRec(m1, s) && checkMetroRec(m2, s)
          }
        }
      }
      checkMetroRec(m, Nil())
    }
  }
}