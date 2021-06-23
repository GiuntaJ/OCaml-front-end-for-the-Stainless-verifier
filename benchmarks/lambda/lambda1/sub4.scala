import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub4 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def cm(m, l) = {
        m match {
          case STATION(n) => { if (l.contains(n)) true else false }
          case AREA(n, m) => { cm(m, if (l.contains(n)) l else n :: l) }
          case CONNECT(m1, m2) => { cm(m1, l) && cm(m2, l) }
        }
      }
      cm(m, Nil())
    }
  }
}