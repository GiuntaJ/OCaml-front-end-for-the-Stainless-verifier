import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub453 {
  /* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-4.ml */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def m_list(((arr: List[Name], m: Metro))): Boolean = {
    m match {
      case STATION(n) => { arr.contains(n) }
      case AREA(n, m_in) => { m_list(arr ++(List(n)), m_in) }
      case CONNECT(m1, m2) => { m_list(arr, m1) && m_list(arr, m2) }
    }
  }
  
  val checkMetro: Metro => Boolean = (
    (x) =>
      {
        val _4 = {
          val empty: List[Name] = Nil()
          m_list(empty, x)
        }
    }
  )
}