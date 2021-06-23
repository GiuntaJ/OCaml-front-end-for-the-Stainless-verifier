import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub54 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def check(((metro, lst))) = {
    metro match {
      case AREA(name, m) => { check(m, name :: lst) }
      case STATION(x) => { lst.contains(x) }
      case CONNECT(a, b) => { check(a, lst) && check(b, lst) }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = { check(metro, Nil()) }
}
