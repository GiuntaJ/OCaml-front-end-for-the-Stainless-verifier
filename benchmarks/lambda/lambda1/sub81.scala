import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub81 {
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checklist(((me, li))) = {
    me match {
      case STATION(n) => { li.contains(n) }
      case AREA(n1, me1) => { checklist(me1, n1 :: li) }
      case CONNECT(me1, me2) => { checklist(me1, li) && checklist(me2, li) }
    }
  }
  
  
  def checkMetro(me: Metro): Boolean = {
    val _2 = {
      val l = Nil()
      checklist(me, l)
    }
  }
  
}
