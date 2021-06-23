import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub409 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def check_list(((l: List[Name], id))): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd != id) check_list(tl, id) else true }
    }
  }
   def checkMetro_list(((l: List[Name], m: Metro))): Boolean = {
    m match {
      case STATION(id) => { if (check_list(l, id)) true else false }
      case AREA(id, m) => { checkMetro_list(id :: l, m) }
      case CONNECT(m1, m2) => { checkMetro_list(l, m1) && checkMetro_list(l, m2)
      }
    }
  }
  
    def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(id) => { false }
      case AREA(id, m) => { checkMetro_list(List(id), m) }
      case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
    }
  }
      
}