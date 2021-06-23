import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub359 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkmetrolist(ametro: Metro, checklist: List[Name]): Boolean = {
    ametro match {
      case STATION(a) => { checklist.contains(a) }
      case AREA(a, b) => { checkmetrolist(b, a :: checklist) }
      case CONNECT(a, b) => {
        checkmetrolist(a, checklist) && checkmetrolist(b, checklist)
      }
    }
  }
  
  def checkMetro(ametro: Metro): Boolean = { checkmetrolist(ametro, Nil()) }
  
}
