import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub319 {
  /* SNU Programming Language Fall 2015
   * Homework 2 
   * Exercise 3: checkMetro
   * Written by Dongho Kang 
   */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def checkMetro_sub: (Metro, List[Name]) => Boolean = {
            case (m, l) =>
              {
                m match {
                  case STATION(n) => { l.contains(n) }
                  case AREA(n, m_n) => { checkMetro_sub(m_n, n :: l) }
                  case CONNECT(m_1, m_2) => {
                    checkMetro_sub(m_1, l) && checkMetro_sub(m_2, l)
                  }
                }
            }
          }
          checkMetro_sub(m, Nil())
        }
    }
  )
}
