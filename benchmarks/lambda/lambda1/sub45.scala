import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub45 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(m_: Metro): Boolean = {
    val _2 = {
      def p(n1, n2) = { n1 == n2 }
      val _3 = {
        def mChecker(m, alst) = {
          m match {
            case STATION(n) => { alst.exists(p(n)) }
            case AREA(n, met) => { mChecker(met, List(n) ++ alst) }
            case CONNECT(met1, met2) => {
              mChecker(met1, alst) && mChecker(met2, alst)
            }
          }
        }
        mChecker(m_, Nil())
      }
    }
  }
}
