import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub188 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def addToList[A](lst: List[A], newInlst: A): List[A] = { newInlst :: lst }
  def check(prevAreaName: List[Name], metro: Metro): Boolean = {
    metro match {
      case STATION(stationName) => { prevAreaName.contains(stationName) }
      case AREA(areaName, nextMetro) => {
        check(areaName :: prevAreaName, nextMetro)
      }
      case CONNECT(metroA, metroB) => {
        check(prevAreaName, metroA) && check(prevAreaName, metroB)
      }
    }
  }
  
  def checkMetro(inpt: Metro): Boolean = {
    inpt match {
      case STATION(name) => { true }
      case AREA(name, metro) => { check(List(name), metro) }
      case CONNECT(metroA, metroB) => { checkMetro(metroA) && checkMetro(metroB)
      }
    }
  }
}