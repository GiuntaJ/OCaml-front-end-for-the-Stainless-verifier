import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub471 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(x: Metro): Boolean = {
    val _2 = {
      def checkMetroRec(x: Metro, y: List[Name]): Boolean = {
        x match {
          case STATION(a) => { y.exists(( (x) => { x == a } )) }
          case AREA(a, b) => { checkMetroRec(b, a :: y) }
          case CONNECT(a, b) => { checkMetroRec(a, y) && checkMetroRec(b, y) }
        }
      }
      checkMetroRec(x, Nil())
    }
  }
}