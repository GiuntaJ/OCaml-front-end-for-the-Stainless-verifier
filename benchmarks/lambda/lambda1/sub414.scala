import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub414 {
  /* 2012-11230 Kim sangmin */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (x) =>
      {
        val _4 = {
          def helper: (Metro, List[String]) => Boolean = {
            case (met, env) =>
              {
                met match {
                  case STATION(n) => { if (env.contains(n)) true else false }
                  case AREA(n, m) => {
                    if (env.contains(n)) helper(m, env) else helper(m, n :: env)
                  }
                  case CONNECT(m1, m2) => {
                    if (helper(m1, env) && helper(m2, env)) true else false
                  }
                }
            }
          }
          helper(x, Nil())
        }
    }
  )
  
}
