import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub92 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkArea(a) = {
        a match {
          case (l, STATION(name)) => { l.contains(name) }
          case (l, CONNECT(m1, m2)) => { checkArea(l, m1) && checkArea(l, m2) }
          case (l, AREA(name, metro)) => { checkArea(name :: l, metro) }
        }
      }
      m match {
        case STATION(_) => { false }
        case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
        case AREA(name, submetro) => { checkArea(List(name), submetro) }
      }
    }
  }
}