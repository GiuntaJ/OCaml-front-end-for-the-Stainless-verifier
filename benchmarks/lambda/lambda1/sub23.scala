import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub23 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def check(((metro, alist))) = {
    metro match {
      case STATION(id) => { alist.contains(id) }
      case AREA(id, m) => { check(m, id :: alist) }
      case CONNECT(m1, m2) => { check(m1, alist) && check(m2, alist) }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = { check(metro, Nil()) }
}