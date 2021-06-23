import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub69 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def sub(((l, a))) = {
    l match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => { if (hd == a) sub(tl, a) else hd :: sub(tl, a) }
    }
  }
  
  def concat(((a, b))) = {
    a match {
      case Nil() => { b }
      case Cons(hd, tl) => { hd :: concat(tl, b) }
    }
  }
  
  def listsub(m: Metro): List[Name] = {
    m match {
      case STATION(a) => { List(a) }
      case AREA(a, b) => { sub(listsub(b), a) }
      case CONNECT(a, b) => { concat(listsub(a), listsub(b)) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(a) => { false }
      case AREA(a, b) => { if (sub(listsub(b), a) == Nil()) true else false }
      case CONNECT(a, b) => { false }
    }
  }
  
  
  
  
}
