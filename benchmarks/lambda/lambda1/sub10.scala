import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub10 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetro2(m, l) = {
        m match {
          case AREA(n, m2) => {
            if (l.contains(n)) checkMetro2(m2, l) else checkMetro2(m2, n :: l)
          }
          case STATION(n) => { if (l.contains(n)) true else false }
          case CONNECT(m1, m2) => {
            if (checkMetro2(m1, l)) checkMetro2(m2, l) else false
          }
        }
      }
      checkMetro2(m, Nil())
    }
  }
  	
}
