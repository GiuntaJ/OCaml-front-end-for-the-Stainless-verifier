import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub221 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def isTrue(m, l) = {
        m match {
          case STATION(n) => { if (l.contains(n)) true else false }
          case AREA(n, mm) => {
            if (l.contains(n)) isTrue(mm, l) else isTrue(mm, List(n) ++ l)
          }
          case CONNECT(mm1, mm2) => { isTrue(mm1, l) && isTrue(mm2, l) }
        }
      }
      m match {
        case STATION(n) => { false }
        case AREA(n, mm) => { isTrue(mm, List(n)) }
        case CONNECT(mm1, mm2) => { isTrue(mm1, Nil()) && isTrue(mm2, Nil()) }
      }
    }
  }
}