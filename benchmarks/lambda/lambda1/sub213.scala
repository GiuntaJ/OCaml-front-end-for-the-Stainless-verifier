import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub213 {
  /*
   * 컴퓨터공학부 2009-11690 김찬민
   * Homework 2 Exercise 1  */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(wholeMap: Metro): Boolean = {
    val _2 = {
      def checkSubMetro(curMap, areaStack) = {
        curMap match {
          case STATION(stationName) => {
            areaStack.exists(( (areaName) => { areaName == stationName } ))
          }
          case AREA(areaName, subMap) => {
            checkSubMetro(subMap, areaName :: areaStack)
          }
          case CONNECT(sub1, sub2) => {
            checkSubMetro(sub1, areaStack) && checkSubMetro(sub2, areaStack)
          }
        }
      }
      checkSubMetro(wholeMap, Nil())
    }
  }
}