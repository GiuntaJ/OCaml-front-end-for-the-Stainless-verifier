import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub267 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def check(((metro, lst))) = {
    metro match {
      case STATION(name) => { lst.contains(name) }
      case CONNECT(metro1, metro2) => { check(metro1, lst) && check(metro2, lst)
      }
      case AREA(name, metro) => { check(metro, List(name) ++(lst)) }
    }
  }
  			
  def checkMetro(metro) = { check(metro, Nil()) }
}
