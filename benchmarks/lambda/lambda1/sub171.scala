import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub171 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def calarea(((lst, st))) = {
    (lst, st) match {
      case (Nil(), _) => { false }
      case (Cons(hd, tl), p) => { if (hd == p) true else calarea(tl, p) }
    }
  }
  
  def mycheck(((m, lst))) = {
    m match {
      case STATION(p) => { calarea(lst, p) }
      case AREA(p, q) => { mycheck(q, lst ++ List(p)) }
      case CONNECT(p, q) => { mycheck(p, lst) && mycheck(q, lst) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      val lst = Nil()
      mycheck(m, lst)
    }
  }
}
