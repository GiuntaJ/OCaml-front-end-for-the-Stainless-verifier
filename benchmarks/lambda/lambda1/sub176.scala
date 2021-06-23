import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub176 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def checkAux(met, area) = {
        met match {
          case STATION(name) => { area.contains(name) }
          case AREA(name, metro) => { checkAux(metro, name :: area) }
          case CONNECT(m1, m2) => { checkAux(m1, area) && checkAux(m2, area) }
        }
      }
      checkAux(metro, Nil())
    }
  }
}