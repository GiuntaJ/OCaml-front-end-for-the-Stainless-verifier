import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub264 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def contains[A](listOfArea: List[A], s: A): Boolean = {
    listOfArea match {
      case Nil() => { false }
      case Cons(h, t) => { if (h == s) true else contains(t, s) }
    }
  }
  
  def checkMetro2(x: Metro, listOfArea: List[Name]): Boolean = {
    x match {
      case STATION(s) => { contains(listOfArea, s) }
      case AREA(n, m) => { checkMetro2(m, n :: listOfArea) }
      case CONNECT(m1, m2) => {
        checkMetro2(m1, listOfArea) && checkMetro2(m2, listOfArea)
      }
    }
  }
  
  def checkMetro(x: Metro): Boolean = {
    x match {
      case STATION(s) => { true }
      case _ => { checkMetro2(x, Nil()) }
    }
  }
}