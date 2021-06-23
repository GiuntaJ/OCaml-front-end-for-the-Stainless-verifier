import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub468 {
  /*컴퓨터공학부 2014-16775 김민지
  programming language hw 2-4*/
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkHelp(sl: List[String], m: Metro): Boolean = {
    m match {
      case STATION(m1) => { if (sl.contains(m1)) true else false }
      case AREA(n, m1) => { checkHelp(n :: sl, m1) }
      case CONNECT(m1, m2) => { checkHelp(sl, m1) && checkHelp(sl, m2) }
    }
  }
  
  def checkMetro(x: Metro): Boolean = { checkHelp(Nil(), x) }
}
