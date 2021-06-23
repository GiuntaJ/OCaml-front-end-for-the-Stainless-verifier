import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub366 {
   sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroWList(((ilist: List[String], met: Metro))): Boolean = {
    (ilist, met) match {
      case (lst, AREA(id, m)) => { checkMetroWList(id :: lst, m) }
      case (lst, STATION(id)) => { lst.contains(id) }
      case (lst, CONNECT(m1, m2)) => {
        checkMetroWList(lst, m1) && checkMetroWList(lst, m2)
      }
    }
  }
  
  def checkMetro(met: Metro): Boolean = { checkMetroWList(Nil(), met) }
}
