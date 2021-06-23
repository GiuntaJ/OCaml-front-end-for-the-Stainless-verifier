import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub94 {
  /* 8 checkMetro: metro -> bool */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def cMetro(metro, areas) = {
        metro match {
          case STATION(n) => { areas.contains(n) }
          case AREA(n, m) => { cMetro(m, n :: areas) }
          case CONNECT(m0, m1) => { cMetro(m0, areas) && cMetro(m1, areas) }
        }
      }
      cMetro(metro, Nil())
    }
  }
}