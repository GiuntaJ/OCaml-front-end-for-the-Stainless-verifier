import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub144 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def my_check(metro: Metro, check: List[Name]): Boolean = {
    metro match {
      case STATION(x) => { check.contains(x) }
      case AREA(name, met) => { my_check(met, check ++ List(name)) }
      case CONNECT(met1, met2) => {
        if (my_check(met1, check) == true) my_check(met2, check) else false
      }
    }
  }
  
  
  
  def checkMetro(m: Metro): Boolean = { my_check(m, Nil()) }
}