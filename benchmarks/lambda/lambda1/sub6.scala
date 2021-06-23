import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub6 {
  /* 2007-11651 KIM DONG HYUN */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def getStn(m) = {
        m match {
          case STATION(name) => { List(name) }
          case AREA(n, m_0) => { getStn(m_0).filter(( (x) => { x ne n } )) }
          case CONNECT(m1, m2) => { getStn(m1) ++ getStn(m2) }
        }
      }
      if (getStn(metro) == Nil()) true else false
    }
  }
}