import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub385 {
  
  /* exercise 4 not yet*/
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkMetro(largein: Metro): Boolean = {
    val _2 = {
      def metrorec(smallin, l) = {
        smallin match {
          case STATION(x) => { l.contains(x) }
          case AREA(x, y) => { metrorec(y, x :: l) }
          case CONNECT(y, z) => { metrorec(y, l) && metrorec(z, l) }
        }
      }
      metrorec(largein, Nil())
    }
  }
}