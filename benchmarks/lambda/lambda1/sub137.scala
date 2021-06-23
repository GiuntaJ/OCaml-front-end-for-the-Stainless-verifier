import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub137 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkMet(met, temp) = {
        met match {
          case STATION(n1) => { temp.contains(n1) }
          case AREA(n1, n2) => { checkMet(n2, n1 :: temp) }
          case CONNECT(n1, n2) => { checkMet(n1, temp) && checkMet(n2, temp) }
        }
      }
      checkMet(met, Nil())
    }
  } 
}
