import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub249 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro_(namelist: List[Name], m: Metro): Boolean = {
    m match {
      case STATION(id) => { namelist.contains(id) }
      case AREA(id, m) => { checkMetro_(id :: namelist, m) }
      case CONNECT(m1, m2) => {
        checkMetro_(namelist, m1) && checkMetro_(namelist, m2)
      }
    }
  }
  	
  def checkMetro(m: Metro): Boolean = { checkMetro_(Nil(), m) }
}