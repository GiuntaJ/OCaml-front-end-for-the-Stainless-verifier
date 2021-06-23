import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub104 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkMetroTmp(areaList, metTemp) = {
        metTemp match {
          case STATION(a) => { areaList.contains(a) }
          case AREA(a, b) => { checkMetroTmp(a :: areaList, b) }
          case CONNECT(a, b) => {
            checkMetroTmp(areaList, a) && checkMetroTmp(areaList, b)
          }
        }
      }
      met match {
        case STATION(a) => { false }
        case AREA(a, b) => { checkMetroTmp(List(a), b) }
        case CONNECT(a, b) => { checkMetro(a) && checkMetro(b) }
      }
    }
  }
}