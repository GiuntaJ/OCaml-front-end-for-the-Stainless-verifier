import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub405 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def op(((l: List[String], a: Metro))) = {
        a match {
          case STATION(n) => { l.exists(( (x) => { x == n } )) }
          case AREA(n, a_0) => { op(List(n) ++(l), a_0) }
          case CONNECT(a_0, a__0) => { op(l, a_0) && op(l, a__0) }
        }
      }
      op(Nil(), m)
    }
  }
}