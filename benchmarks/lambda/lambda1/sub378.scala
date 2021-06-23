import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub378 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  type AreaList = List[String]
  
  def isIncluded(n: Name, al: AreaList): Boolean = {
    al match {
      case Nil() => { false }
      case Cons(hal, tal) => { if (hal == n) true else isIncluded(n, tal) }
    }
  }
  
  def scan(m: Metro, al: AreaList): Boolean = {
    m match {
      case STATION(n) => { isIncluded(n, al) }
      case AREA(n, mm) => { scan(mm, al ++ List(n)) }
      case CONNECT(m1, m2) => { scan(m1, al) && scan(m2, al) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { scan(m, Nil()) }
}