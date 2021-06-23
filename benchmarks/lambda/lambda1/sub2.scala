import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub2 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro) = {
    val _2 = {
      def check(((ma: Metro, li: List[Name]))) = {
        ma match {
          case AREA(st, k) => { check(k, st :: li) }
          case CONNECT(me1, me2) => { check(me1, li) && check(me2, li) }
          case STATION(na) => { li.contains(na) }
        }
      }
      check(m, Nil())
    }
  }
}