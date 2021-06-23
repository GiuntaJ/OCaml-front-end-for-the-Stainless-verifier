import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub412 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkList(((met, l))) = {
    met match {
      case STATION(n) => { if (l.contains(n)) true else false }
      case AREA(n, m) => { checkList(m, List(n) ++ l) }
      case CONNECT(m1, m2) => { checkList(m1, l) && checkList(m2, l) }
    }
  }
  
  def checkMetro(met: Metro): Boolean = { checkList(met, Nil()) }
  
}
