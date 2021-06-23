import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub250 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checking(met: Metro, env: List[Name]): Boolean = {
    met match {
      case STATION(st) => { env.contains(st) }
      case AREA(ar, m) => { checking(m, ar :: env) }
      case CONNECT(m1, m2) => { checking(m1, env) && checking(m2, env) }
    }
  }
  
  def checkMetro(met: Metro): Boolean = { checking(met, Nil()) }
  	
  
  	
}