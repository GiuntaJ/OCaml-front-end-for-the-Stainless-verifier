import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub373 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (input) =>
      {
        val _4 = {
          def checkMetro2: (Metro, List[Name]) => Boolean = {
            case (input, names) =>
              {
                input match {
                  case STATION(n) => { names.contains(n) }
                  case AREA(n, m) => { checkMetro2(m, n :: names) }
                  case CONNECT(m1, m2) => {
                    checkMetro2(m1, names) && checkMetro2(m2, names)
                  }
                }
            }
          }
          checkMetro2(input, Nil())
        }
    }
  )
}