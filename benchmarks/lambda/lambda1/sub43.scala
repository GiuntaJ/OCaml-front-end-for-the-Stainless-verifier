import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub43 {
  /* 4190.310 Programming Language			*
   * Homework #1 - Exercise 7 (CheckMetroMap)	*
   * 2008-11744 Jongwook Choi 				*/
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def checkMetroAux(env, m) = {
            m match {
              case STATION(name) => { env.exists(( (t) => { t == name } )) }
              case AREA(name, m_0) => { checkMetroAux(name :: env, m_0) }
              case CONNECT(m1, m2) => {
                checkMetroAux(env, m1) && checkMetroAux(env, m2)
              }
            }
          }
          checkMetroAux(Nil(), m)
        }
    }
  )
}
