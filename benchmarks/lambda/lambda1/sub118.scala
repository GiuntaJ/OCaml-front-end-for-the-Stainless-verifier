import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub118 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def __checkMetro(((m, l))) = {
    m match {
      case STATION(a) => { if (l.exists(( (x) => { x == a } ))) true else false
      }
      case AREA(_name, _metro) => { __checkMetro(_metro, _name :: l) }
      case CONNECT(m1, m2) => { __checkMetro(m1, l) && __checkMetro(m2, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { __checkMetro(m, Nil()) }
}