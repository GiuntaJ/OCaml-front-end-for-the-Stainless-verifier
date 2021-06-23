import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub300 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (met) =>
      {
        val _4 = {
          def checkMetroIn: (Metro, List[Name]) => Boolean = {
            case (metro, stationlist) =>
              {
                metro match {
                  case STATION(stationstr) => {
                    if (stationlist.contains(stationstr)) true else false
                  }
                  case AREA(areastr, metin) => {
                    checkMetroIn(metin, stationlist ++ List(areastr))
                  }
                  case CONNECT(metin1, metin2) => {
                    checkMetroIn(metin1, stationlist) &&
                    checkMetroIn(metin2, stationlist)
                  }
                }
            }
          }
          met match {
            case STATION(namestr) => { true }
            case _ => { checkMetroIn(met, Nil()) }
          }
        }
    }
  )
}