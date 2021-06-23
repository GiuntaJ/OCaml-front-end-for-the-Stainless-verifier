import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub365 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def loop(area, metro) = {
        metro match {
          case STATION(name) => { area.exists(( (x) => { x == name } )) }
          case AREA(name, metro_0) => { loop(name :: area, metro_0) }
          case CONNECT(metro1, metro2) => {
            loop(area, metro1) && loop(area, metro2)
          }
        }
      }
      loop(Nil(), m)
    }
  }
}
