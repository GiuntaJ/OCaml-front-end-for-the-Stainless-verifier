import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub430 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val arealist: List[String] = Nil()
  
  def checkarea(((a: List[String], b: Metro))): Boolean = {
    b match {
      case STATION(nm) => { a.contains(nm) }
      case AREA(nm, mt) => { checkarea(nm :: a, mt) }
      case CONNECT(m1, m2) => { checkarea(a, m1) && checkarea(a, m2) }
    }
  }
  
  def checkMetro(b: Metro): Boolean = { checkarea(arealist, b) }
}