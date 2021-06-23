import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub287 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def iter(target, names) = {
        target match {
          case STATION(station_name) => {
            names.filter(( (x) => { x == station_name } )).length > 0
          }
          case AREA(area_name, new_target) => {
            iter(new_target, area_name :: names)
          }
          case CONNECT(metro_a, metro_b) => {
            iter(metro_a, names) && iter(metro_b, names)
          }
        }
      }
      iter(metro, Nil())
    }
  }
}