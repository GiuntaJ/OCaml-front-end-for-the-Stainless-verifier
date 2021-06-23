import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub472 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
    
    def checkMetro2: (Metro, List[Name]) => Boolean = {
    case (current_metro, area_list) =>
      {
        current_metro match {
          case AREA(area_name, sub_metro) => {
            checkMetro2(sub_metro, area_name :: area_list)
          }
          case CONNECT(sub_metro_1, sub_metro_2) => {
            checkMetro2(sub_metro_1, area_list) &&
            checkMetro2(sub_metro_2, area_list)
          }
          case STATION(station_name) => { area_list.contains(station_name) }
        }
    }
  }
            
            
    val checkMetro: Metro => Boolean = ( (_metro) => { checkMetro2(_metro, Nil()) } )
      
}