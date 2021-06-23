import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub114 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def is_in(lst, sta) = { lst.contains(sta) }
      val _3 = {
        def check(m, lst) = {
          (m, lst) match {
            case (STATION(a), lst) => { is_in(lst, a) }
            case (AREA(a, b), lst) => { check(b, a :: lst) }
            case (CONNECT(a, b), lst) => { check(a, lst) && check(b, lst) }
          }
        }
        check(m, Nil())
      }
    }
  }
}