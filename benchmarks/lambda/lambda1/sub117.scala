import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub117 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def check(m, area) = {
        m match {
          case STATION(name) => { area.contains(name) }
          case AREA(name, m1) => { check(m1, name :: area) }
          case CONNECT(m1, m2) => { check(m1, area) && check(m2, area) }
        }
      }
      check(m, Nil())
    }
  }
}