import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub143 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def in_list[A](el: A, lst: List[A]): Boolean = {
    lst match {
      case Nil() => { false }
      case Cons(head, tail) => { head == el || in_list(el, tail) }
    }
  }
  
  def checkMetro_sub(metro: Metro, lst: List[Name]): Boolean = {
    metro match {
      case STATION(s_name) => { in_list(s_name, lst) }
      case AREA(a_name, metro_sub) => { checkMetro_sub(metro_sub, a_name :: lst)
      }
      case CONNECT(a, b) => { checkMetro_sub(a, lst) && checkMetro_sub(b, lst) }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = { checkMetro_sub(metro, Nil()) }
}
