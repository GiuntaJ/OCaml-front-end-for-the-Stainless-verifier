import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub237 {
  sealed case class TODO() extends Exception {} /*done done*/
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def listSearch(((l: List[String], key: String))): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == key) true else listSearch(tl, key) }
    }
  }
  
  def checkNameStation(((n: List[String], m: Metro))): Boolean = {
    m match {
      case STATION(sname) => { listSearch(n, sname) }
      case AREA(sname, subm) => { checkNameStation(sname :: n, subm) }
      case CONNECT(subm1, subm2) => {
        checkNameStation(n, subm1) && checkNameStation(n, subm2)
      }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(n) => { false }
      case AREA(n, STATION(sname)) => { if (n == sname) true else false }
      case AREA(n, subm) => { checkNameStation(List(n), subm) }
      case CONNECT(subm1, subm2) => { checkMetro(subm1) && checkMetro(subm2) }
    }
  }
}