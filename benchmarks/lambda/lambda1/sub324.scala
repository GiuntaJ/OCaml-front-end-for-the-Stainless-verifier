import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub324 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def findStation: Metro => List[String] = (
    (x) =>
      {
        x match {
          case STATION(station) => { List(station) }
          case CONNECT(a, b) => { findStation(a) ++ findStation(b) }
          case AREA(area, next) => { findStation(next) }
        }
    }
  )
  
  def exclude: (Metro, List[String]) => Boolean = {
    case (m, strl) =>
      {
        strl match {
          case Nil() => { true }
          case _ => {
            m match {
              case STATION(sta) => { false }
              case AREA(area, next) => {
                
                  if (
                    strl.contains(area)
                  ) {
                    exclude(next, strl.filter(( (x) => { x ne area } ))) 
                  } else {
                    exclude(next, strl)
                  }
              }
              case CONNECT(STATION(sta), con2) => { exclude(con2, strl) }
              case CONNECT(con1, STATION(sta)) => { exclude(con1, strl) }
              case CONNECT(con1, con2) => {
                exclude(con1, findStation(con1)) &&
                exclude(con2, findStation(con2))
              }
            }
          }
        }
    }
  }
  
  def checkMetro: Metro => Boolean = ( (x) => { exclude(x, findStation(x)) } )
}