import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub371 {
  /*
      Homework 2, Exercise 4
      2015-15894 Jonghoon Won
      Sep 28, 2017
  */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = val _0 = {
    def checkInList: (A, List[A]) => Boolean = {
      case (value, lst) =>
        {
          lst match {
            case Nil() => { false }
            case Cons(hd, tl) => {
              if (value == hd) true else checkInList(value, tl)
            }
          }
      }
    }
    val _1 = {
      def checkMetroHelper: (List[String], Metro) => Boolean = {
        case (areaList, met) =>
          {
            met match {
              case STATION(stationName) => { checkInList(stationName, areaList)
              }
              case AREA(areaName, nextMetro) => {
                
                  if (
                    checkInList(areaName, areaList)
                  ) {
                    checkMetroHelper(areaList, nextMetro) 
                  } else {
                    checkMetroHelper(areaName :: areaList, nextMetro)
                  }
              }
              case CONNECT(metro1, metro2) => {
                checkMetroHelper(areaList, metro1) &&
                checkMetroHelper(areaList, metro2)
              }
            }
        }
      }
      ( (met) => { checkMetroHelper(Nil(), met) } )
    }
  }
}