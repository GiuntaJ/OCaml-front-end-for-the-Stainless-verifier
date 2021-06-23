import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub112 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(mtr: Metro): Boolean = {
    val _2 = {
      def cM(mtr, lst) = {
        mtr match {
          case STATION(a) => { lst.contains(a) }
          case AREA(a, m) => { cM(m, a :: lst) }
          case CONNECT(a, b) => { cM(a, lst) && cM(b, lst) }
        }
      }
      cM(mtr, Nil())
    }
  }
}