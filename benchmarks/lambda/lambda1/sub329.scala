import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub329 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro2(someMetro: Metro, chkList: List[Name]): Boolean = {
    someMetro match {
      case STATION(u) => { chkList.contains(u) }
      case AREA(u, v) => { checkMetro2(v, u :: chkList) }
      case CONNECT(u, v) => { checkMetro2(u, chkList) && checkMetro2(v, chkList)
      }
    }
  }
  
  def checkMetro(someMetro: Metro): Boolean = { checkMetro2(someMetro, Nil()) }
}