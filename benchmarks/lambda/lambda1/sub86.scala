import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub86 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (startm) =>
      {
        val _4 = {
          def checkMetro_0: (Metro, List[Name]) => Boolean = {
            case (m, l) =>
              {
                m match {
                  case STATION(n) => { l.contains(n) }
                  case AREA(n1, m2) => {
                    
                      if (
                        l.contains(n1) == false
                      ) {
                        checkMetro_0(m2, n1 :: l) 
                      } else {
                        checkMetro_0(m2, l)
                      }
                  }
                  case CONNECT(m1, m2) => {
                    checkMetro_0(m1, l) && checkMetro_0(m2, l)
                  }
                }
            }
          }
          val _5 = {
            val startl: List[Name] = Nil()
            checkMetro_0(startm, startl)
          }
        }
    }
  )
}