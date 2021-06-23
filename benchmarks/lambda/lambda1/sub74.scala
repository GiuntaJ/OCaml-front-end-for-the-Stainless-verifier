import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub74 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def check(((metro, discovered))) = {
    metro match {
      case STATION(n) => { discovered.contains(n) }
      case AREA(n, m) => { check(m, n :: discovered) }
      case CONNECT(m1, m2) => { check(m1, discovered) && check(m2, discovered) }
    }
  }
  
  def checkMetro(metro) = { check(metro, Nil()) }
  
  	
}