import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub252 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(arg1: Metro): Boolean = {
    val _2 = {
      def check(arg2, my_list) = {
        arg2 match {
          case STATION(n) => { my_list.contains(n) }
          case AREA(a, b) => { check(b, my_list ++(List(a))) }
          case CONNECT(c, d) => { check(c, my_list) && check(d, my_list) }
        }
      }
      check(arg1, Nil())
    }
  }
}