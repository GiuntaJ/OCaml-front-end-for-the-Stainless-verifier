import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub210 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(input: Metro): Boolean = {
    val _2 = {
      def checkStation(((areaList, station_name))) = {
        if (areaList.exists(( (x) => { x == station_name } ))) true else false
      }
      val _3 = {
        def checkArea(((areaList, m))) = {
          m match {
            case STATION(value) => { checkStation(areaList, value) }
            case AREA(value, m_prime) => {
              checkArea(areaList ++ List(value), m_prime)
            }
            case CONNECT(first, second) => {
              checkArea(areaList, first) && checkArea(areaList, second)
            }
          }
        }
        input match {
          case STATION(value) => { checkStation(Nil(), value) }
          case AREA(value, second) => { checkArea(List(value), second) }
          case CONNECT(first, second) => {
            checkMetro(first) && checkMetro(second)
          }
        }
      }
    }
  }
}