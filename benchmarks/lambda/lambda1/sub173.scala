import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub173 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroByList(((m, l))) = {
    m match {
      case STATION(a) => { l.exists(( (x) => { x == a } )) }
      case AREA(n, a) => { checkMetroByList(a, l ++ List(n)) }
      case CONNECT(a, b) => { checkMetroByList(a, l) && checkMetroByList(b, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetroByList(m, Nil()) }
}