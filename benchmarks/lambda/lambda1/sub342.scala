import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub342 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  val checkMetro: Metro => Boolean = (
    (metroLine) =>
      {
        val _4 = {
          def lstContains(lst, areaName) = {
            lst match {
              case Nil() => { false }
              case Cons(h, rest) => {
                if (h == areaName) true else lstContains(rest, areaName)
              }
            }
          }
          val _5 = {
            def checkMetroArea(((metroLine, areaLst))) = {
              metroLine match {
                case STATION(stationName) => { lstContains(areaLst, stationName)
                }
                case AREA(areaName, subMetro) => {
                  checkMetroArea(subMetro, areaLst ++ List(areaName))
                }
                case CONNECT(metro1, metro2) => {
                  checkMetroArea(metro1, areaLst) &&
                  checkMetroArea(metro2, areaLst)
                }
              }
            }
            checkMetroArea(metroLine, Nil())
          }
        }
    }
  )
}