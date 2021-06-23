import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub41 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def add_element[A](x: A, set: List[A]): List[A] = { x :: set }
  
  def check[A](x: A, set: List[A]): Boolean = {
    if (set.contains(x)) true else false
  }
  
  
  
  def sub_checkMetro(metro: Metro, set: List[Name]): Boolean = {
    metro match {
      case AREA(x, m) => { sub_checkMetro(m, add_element(x, set)) }
      case STATION(n) => { check(n, set) }
      case CONNECT(m1, m2) => {
        sub_checkMetro(m1, set) && sub_checkMetro(m2, set)
      }
    }
  }
  	
  
  def checkMetro(metro: Metro): Boolean = { sub_checkMetro(metro, Nil()) }
}