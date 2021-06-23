import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub322 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String 
  
  def inner_check: (List[Name], Metro) => Boolean = {
    case (l, met) =>
      {
        met match {
          case STATION(name) => { l.contains(name) }
          case CONNECT(m1, m2) => { inner_check(l, m1) && inner_check(l, m2) }
          case AREA(name, m) => { inner_check(name :: l, m) }
        }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { inner_check(Nil(), m) }	
}