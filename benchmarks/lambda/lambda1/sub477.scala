import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub477 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def matchid(((x: String, l: List[String]))): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == x) true else matchid(x, tl) }
    }
  }
  
  def ckMetro(((m: Metro, idl: List[String]))): Boolean = {
    m match {
      case STATION(x) => { matchid(x, idl) }
      case AREA(id, STATION(x)) => { ckMetro(STATION(x), id :: idl) }
      case AREA(id, some) => { ckMetro(some, id :: idl) }
      case CONNECT(STATION(x), STATION(y)) => {
        matchid(x, idl) && matchid(y, idl)
      }
      case CONNECT(STATION(x), some) => {
        if (matchid(x, idl)) ckMetro(some, idl) else false
      }
      case CONNECT(x, y) => { ckMetro(x, idl) && ckMetro(y, Nil()) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { ckMetro(m, Nil()) }
  
  
  
}
