import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub423 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(x: Metro): Boolean = {
    val _2 = {
      def checkMetroIter(((x: Metro, nlist: List[String]))): Boolean = {
        x match {
          case STATION(a) => { nlist.contains(a) }
          case AREA(n, m) => { checkMetroIter(m, n :: nlist) }
          case CONNECT(a, b) => {
            checkMetroIter(a, nlist) && checkMetroIter(b, nlist)
          }
        }
      }
      checkMetroIter(x, Nil())
    }
  }
}