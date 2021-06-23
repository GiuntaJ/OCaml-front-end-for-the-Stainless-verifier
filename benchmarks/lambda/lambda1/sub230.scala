import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub230 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  
  def checkNameList(((l, n))) = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == n) true else checkNameList(tl, n) }
    }
  }
  
  def checkMetroRec(((l, m))) = {
    m match {
      case STATION(n) => { checkNameList(l, n) }
      case AREA(n, m0) => { checkMetroRec(n :: l, m0) }
      case CONNECT(m0, m1) => { checkMetroRec(l, m0) && checkMetroRec(l, m1) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(n) => { false }
      case AREA(n, m0) => { checkMetroRec(List(n), m0) }
      case CONNECT(m0, m1) => { checkMetro(m0) && checkMetro(m1) }
    }
  }
}