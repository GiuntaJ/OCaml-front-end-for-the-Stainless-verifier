import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub216 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def compareId(idStation: String, idArea: String): Boolean = {
    if (idStation == idArea) true else false
  }
  
  def checkArea(idList: List[String], m: Metro): Boolean = {
    m match {
      case STATION(id) => { idList.exists(compareId(id)) }
      case AREA(id, m1) => { checkArea(id :: idList, m1) }
      case CONNECT(m1, m2) => { checkArea(idList, m1) && checkArea(idList, m2) }
    }
  }
  
  /* checkMetro: metro -> bool */
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(id) => { false }
      case AREA(id, m1) => { checkArea(List(id), m1) }
      case CONNECT(m1, m2) => { checkArea(Nil(), m1) && checkArea(Nil(), m2) }
    }
  }
}