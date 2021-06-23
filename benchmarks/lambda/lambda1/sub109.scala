import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub109 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def cmModule(metro2, stations) = {
        metro2 match {
          case STATION(name2) => { stations.contains(name2) }
          case AREA(name2, metro3) => { cmModule(metro3, name2 :: stations) }
          case CONNECT(metro3, metro4) => {
            cmModule(metro3, stations) && cmModule(metro4, stations)
          }
        }
      }
      cmModule(metro, Nil())
    }
  }
}