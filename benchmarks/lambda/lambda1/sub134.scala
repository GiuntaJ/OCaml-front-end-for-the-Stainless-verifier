import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub134 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def _check(((m, al))) = {
        m match {
          case STATION(name) => { al.contains(name) }
          case AREA(name, m_0) => { _check(m_0, name :: al) }
          case CONNECT(m1, m2) => { _check(m1, al) && _check(m2, al) }
        }
      }
      _check(m, Nil())
    }
  }
}