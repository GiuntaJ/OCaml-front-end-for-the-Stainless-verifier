import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub122 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def check(lst: List[Name], name: Name, metro: Metro): Boolean = {
    val _2 = {
      def mklst(lst, x) = { x :: lst }
      metro match {
        case STATION(n) => { if (lst.contains(n)) true else false }
        case AREA(n, m) => {
          check(mklst(lst, n), name, m) || check(mklst(lst, n), n, m)
        }
        case CONNECT(m1, m2) => { check(lst, name, m1) && check(lst, name, m2) }
      }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = {
    metro match {
      case STATION(n) => { false }
      case AREA(n, m) => { check(List(n), n, m) }
      case CONNECT(m1, m2) => { false }
    }
  }
}