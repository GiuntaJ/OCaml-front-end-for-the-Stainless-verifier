import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub418 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (met) =>
      {
        val _4 = {
          def helper: (Metro, List[String]) => Boolean = {
            case (m, l) =>
              {
                m match {
                  case STATION(n) => { l.contains(n) }
                  case AREA(n, m) => { helper(m, n :: l) }
                  case CONNECT(m1, m2) => { helper(m1, l) && helper(m2, l) }
                }
            }
          }
          helper(met, Nil())
        }
    }
  )
  		
}