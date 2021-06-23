import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub124 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(mat: Metro): Boolean = {
    val _2 = {
      def chk(mat, stl) = {
        mat match {
          case STATION(a) => { stl.contains(a) }
          case AREA(a, b) => { chk(b, a :: stl) }
          case CONNECT(a, b) => { chk(a, stl) && chk(b, stl) }
        }
      }
      chk(mat, Nil())
    }
  }
  
}
