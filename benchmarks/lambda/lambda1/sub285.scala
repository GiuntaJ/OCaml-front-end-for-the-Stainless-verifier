import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub285 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def check(met, str_list) = {
        met match {
          case STATION(a) => { str_list.contains(a) }
          case AREA(a, STATION(b)) => { a == b || str_list.contains(b) }
          case AREA(a, b) => { check(b, str_list ++ List(a)) }
          case CONNECT(a, b) => { check(a, str_list) && check(b, str_list) }
        }
      }
      check(metro, Nil())
    }
  }
}
