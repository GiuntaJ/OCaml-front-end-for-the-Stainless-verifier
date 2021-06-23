import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub190 {
  /* KIHWAN KANG HW01-4 */
  
  /* PREDEFINED TYPES */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  /* END OF PREDEFINED TYPES */
  
  def checkMetro(one: Metro): Boolean = {
    val _2 = {
      def checkStationWithAreaList(((station, area_list))) = {
        (station, area_list) match {
          case (_, Nil()) => { false }
          case (station, Cons(area_name, area_rest)) => {
            
              if (
                station == area_name
              ) {
                true 
              } else {
                checkStationWithAreaList(station, area_rest)
              }
          }
        }
      }
      val _3 = {
        def checkMetroWithAreaList(((one, area_list))) = {
          one match {
            case STATION(station) => {
              checkStationWithAreaList(station, area_list)
            }
            case AREA(area_name, area_metro) => {
              checkMetroWithAreaList(area_metro, area_name :: area_list)
            }
            case CONNECT(front, rear) => {
              checkMetroWithAreaList(front, area_list) &&
              checkMetroWithAreaList(rear, area_list)
            }
          }
        }
        checkMetroWithAreaList(one, Nil())
      }
    }
  }
}