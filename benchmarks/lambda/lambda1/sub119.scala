import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub119 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (met) =>
      {
        val _4 = {
          def checkName: (List[String], Metro) => Boolean = {
            case (l, m) =>
              {
                m match {
                  case AREA(id, mt) => { checkName(id :: l, mt) }
                  case STATION(id) => { l.contains(id) }
                  case CONNECT(m1, m2) => { checkName(l, m1) && checkName(l, m2)
                  }
                }
            }
          }
          checkName(Nil(), met)
        }
    }
  )
}