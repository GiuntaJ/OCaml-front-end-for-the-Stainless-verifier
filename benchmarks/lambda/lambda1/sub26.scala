import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub26 {
  sealed case class Error(param0: String) extends Exception {}
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (metro) =>
      {
        val _4 = {
          def checkMetro: (Metro, List[Name]) => Boolean = {
            case (metro, env) =>
              {
                metro match {
                  case STATION(n) => { env.contains(n) }
                  case AREA(n, m) => { checkMetro(m, n :: env) }
                  case CONNECT(m1, m2) => {
                    checkMetro(m1, env) && checkMetro(m2, env)
                  }
                }
            }
          }
          checkMetro(metro, Nil())
        }
    }
  )
}