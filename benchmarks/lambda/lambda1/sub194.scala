import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub194 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  
  def checkMetro(met: Metro): Boolean = {
    met match {
      case STATION(n) => { false }
      case AREA(n, metr) => { idSearch(n, idList(metr)) || checkMetro(metr) }
      case CONNECT(metr1, metr2) => { checkMetro(metr1) && checkMetro(metr2) }
    }
  }
  def idList(met) = {
    met match {
      case STATION(n) => { List(n) }
      case AREA(n, metr) => { idList(metr) }
      case CONNECT(metr1, metr2) => { idList(metr1) ++ idList(metr2) }
    }
  }
  def idSearch(((id, l))) = {
    l match {
      case Nil() => { false }
      case Cons(head, tail) => { if (head == id) true else idSearch(id, tail) }
    }
  }
}