import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub374 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def findStringinList(((l: List[String], name: String))): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => {
        if (hd == name) true else findStringinList(tl, name)
      }
    }
  }
  
  def checkMetrowithList(((m: Metro, l: List[String]))): Boolean = {
    m match {
      case STATION(name_) => { if (findStringinList(l, name_)) true else false }
      case AREA(name_, metro_) => { checkMetrowithList(metro_, name_ :: l) }
      case CONNECT(metro_1, metro_2) => {
        checkMetrowithList(metro_1, l) && checkMetrowithList(metro_2, l)
      }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(name_) => { false }
      case AREA(name_, metro_) => { checkMetrowithList(metro_, List(name_)) }
      case CONNECT(metro_1, metro_2) => {
        checkMetrowithList(metro_1, Nil()) && checkMetrowithList(metro_2, Nil())
      }
    }
  }
}
