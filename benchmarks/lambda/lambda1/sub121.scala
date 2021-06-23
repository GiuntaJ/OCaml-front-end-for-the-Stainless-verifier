import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub121 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def check(metro, area) = {
        metro match {
          case STATION(station) => { area.contains(station) }
          case AREA(name, metro) => { check(metro, name :: area) }
          case CONNECT(metro1, metro2) => {
            check(metro1, area) && check(metro2, area)
          }
        }
      }
      check(metro, Nil())
    }
  }
  	
}