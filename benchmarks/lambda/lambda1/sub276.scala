import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub276 {
  /* C:\OCaml\lib\CheckMertoMap.ml */
  
  type Name = String
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def check_list(((lst, name))) = {
    lst match {
      case Nil() => { false }
      case Cons(head, tail) => {
        if (head == name) true else check_list(tail, name)
      }
    }
  }
    
  def list_make(((lst, metro))) = {
    metro match {
      case STATION(a) => { check_list(lst, a) }
      case AREA(a, b) => { list_make(a :: lst, b) }
      case CONNECT(a, b) => { list_make(lst, a) && list_make(lst, b) }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = {
    metro match {
      case STATION(a) => { false }
      case AREA(a, b) => { list_make(List(a), b) }
      case CONNECT(a, b) => { checkMetro(a) && checkMetro(b) }
    }
  }
        
}