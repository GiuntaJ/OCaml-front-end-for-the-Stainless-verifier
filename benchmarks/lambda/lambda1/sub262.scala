import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub262 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def streq[A](a: A, b: A): Boolean = { a == b }
  
  def checkMetro_lst(mtr: Metro, lst: List[Name]): Boolean = {
    mtr match {
      case STATION(x) => { lst.exists(streq(x)) }
      case AREA(x, y) => { checkMetro_lst(y, x :: lst) }
      case CONNECT(x, y) => { checkMetro_lst(x, lst) && checkMetro_lst(y, lst) }
    }
  }
  
  def checkMetro(mtr: Metro): Boolean = { checkMetro_lst(mtr, Nil()) }
}
