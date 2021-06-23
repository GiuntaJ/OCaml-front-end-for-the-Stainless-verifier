import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub52 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def check(((l, m))) = {
    m match {
      case STATION(a) => { l.exists(( (n) => { n == a } )) }
      case AREA(a, b) => { check(a :: l, b) }
      case CONNECT(a, b) => { check(l, a) && check(l, b) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { check(Nil(), m) }
}