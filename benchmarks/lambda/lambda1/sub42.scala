import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub42 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checklst(lst: List[Name], m: Metro): Boolean = {
    m match {
      case STATION(id) => { lst.contains(id) }
      case AREA(id, m1) => { checklst(id :: lst, m1) }
      case CONNECT(m1, m2) => { checklst(lst, m1) && checklst(lst, m2) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checklst(Nil(), m) }
}