import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub367 {
  /*Computer Science Engineering 2015-12683 Kim Jaein*/
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkhelp(((input, a_list))) = {
    input match {
      case STATION(a) => { a_list.exists(( (x) => { x == a } )) }
      case AREA(a, inside) => { checkhelp(inside, List(a) ++(a_list)) }
      case CONNECT(a, b) => { checkhelp(a, a_list) && checkhelp(b, a_list) }
    }
  }
  
  def checkMetro(input: Metro): Boolean = { checkhelp(input, Nil()) }
}
