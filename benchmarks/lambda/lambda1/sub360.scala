import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub360 {
  /* Homework 2 - Exercise 4
   * 2011-10492 Jaeyeong Yang */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def auc: (Metro, List[Name]) => Boolean = {
            case (mm, env) =>
              {
                mm match {
                  case STATION(n) => { env.contains(n) }
                  case AREA(n, ml) => { auc(ml, n :: env) }
                  case CONNECT(m1, m2) => { auc(m1, env) && auc(m2, env) }
                }
            }
          }
          auc(m, Nil())
        }
    }
  )
}