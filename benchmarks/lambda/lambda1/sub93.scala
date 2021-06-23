import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub93 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def isThereID(metro, l) = {
        metro match {
          case STATION(n) => { l.contains(n) }
          case AREA(n, m) => { isThereID(m, n :: l) }
          case CONNECT(m1, m2) => { isThereID(m1, l) && isThereID(m2, l) }
        }
      }
      isThereID(metro, Nil())
    }
  }
}