import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub369 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def withList(((l: List[String], mm: Metro))): Boolean = {
        mm match {
          case STATION(name) => { l.contains(name) }
          case AREA(id, m1) => { withList(id :: l, m1) }
          case CONNECT(m1, m2) => { withList(l, m1) && withList(l, m2) }
        }
      }
      withList(Nil(), m)
    }
  }
}