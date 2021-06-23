import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub187 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def checkcorrect[A](l: (List[A], A)): Boolean = {
    l match {
      case (Nil(), station) => { false }
      case (Cons(a, lst), station) => {
        if (a == station) true else checkcorrect(lst, station)
      }
    }
  }
  def checkMetro2(m: (List[Name], Metro)): Boolean = {
    m match {
      case (lst, AREA(left, right)) => { checkMetro2(left :: lst, right) }
      case (lst, STATION(station)) => { checkcorrect(lst, station) }
      case (lst, CONNECT(left, right)) => {
        checkMetro2(lst, left) && checkMetro2(lst, right)
      }
    }
  }
  def checkMetro(n: Metro): Boolean = { checkMetro2(Nil(), n) }
}