import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub40 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetroRec(avail, m) = {
        m match {
          case STATION(s) => { avail.contains(s) }
          case AREA(n, m1) => { checkMetroRec(n :: avail, m1) }
          case CONNECT(m1, m2) => {
            checkMetroRec(avail, m1) && checkMetroRec(avail, m2)
          }
        }
      }
      checkMetroRec(Nil(), m)
    }
  }
}