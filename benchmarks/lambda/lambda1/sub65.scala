import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub65 {
  /* 컴퓨터공학부/2009-11679/김정명/7 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def iter(((m, l))) = {
        m match {
          case STATION(s) => { l.contains(s) }
          case AREA(a, me) => { iter(me, l ++ List(a)) }
          case CONNECT(m1, m2) => { iter(m1, l) && iter(m2, l) }
        }
      }
      iter(m, Nil())
    }
  }
}