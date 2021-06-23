import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub179 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def list_del[A](lst: List[A], element: A): List[A] = {
    lst match {
      case Nil() => { Nil() }
      case Cons(head, tail) => {
        
          if (
            head == element
          ) {
            list_del(tail, element) 
          } else {
            head :: list_del(tail, element)
          }
      }
    }
  }
  
  def remainStation(m: Metro): List[Name] = {
    m match {
      case STATION(str) => { List(str) }
      case AREA(str, met) => { list_del(remainStation(met), str) }
      case CONNECT(met1, met2) => { remainStation(met1) ++ remainStation(met2) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    if (remainStation(m).length == 0) true else false
  }
}
