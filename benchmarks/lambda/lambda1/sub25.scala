import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub25 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def check(var_metro: Metro): List[Name] = {
    var_metro match {
      case CONNECT(m1, m2) => { check(m1) ++ check(m2) }
      case AREA(n, m) => { check(m).filter(( (x) => { x ne n } )) }
      case STATION(n) => { List(n) }
    }
  }
  def checkMetro(var_metro: Metro): Boolean = { check(var_metro).length eq 0 }
}