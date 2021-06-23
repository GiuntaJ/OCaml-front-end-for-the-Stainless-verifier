import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub195 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = { check(met, Nil()) }
  def check(((met, l))) = {
    met match {
      case STATION(n) => { if (idSearch(n, l)) true else false }
      case AREA(n, metr) => { check(metr, List(n) ++ l) }
      case CONNECT(metr1, metr2) => { check(metr1, l) && check(metr2, l) }
    }
  }
  def idSearch(((id, l))) = {
    l match {
      case Nil() => { false }
      case Cons(head, tail) => { if (head == id) true else idSearch(id, tail) }
    }
  }
}