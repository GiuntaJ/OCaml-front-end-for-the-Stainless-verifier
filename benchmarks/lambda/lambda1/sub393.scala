import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub393 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def helper(alist, m) = {
            m match {
              case STATION(s) => { alist.contains(s) }
              case AREA(n, m_0) => { helper(n :: alist, m_0) }
              case CONNECT(x, y) => { helper(alist, x) && helper(alist, y) }
            }
          }
          helper(Nil(), m)
        }
    }
  )
}