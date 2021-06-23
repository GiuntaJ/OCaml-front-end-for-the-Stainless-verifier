import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub212 {
  /* 프로그래밍언어 HW1 Exercise 2
     2009-11657 김동현 */
  	
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /*
  let rec searchList (elem, lst) =
  	if lst = [] then false
  	else
  	  let (h::t) = lst in
  	  if elem = h then true
  	  else searchList (elem, t)
  */
  
  def searchList(((elem, lst))) = {
    lst match {
      case Nil() => { false }
      case Cons(h, t) => { if (elem == h) true else searchList(elem, t) }
    }
  }
  /* 리스트 내에 원소가 존재하면 true, 없으면 false return */
  
  def checkMetro2(((met, lst))) = {
    met match {
      case STATION(s) => { searchList(s, lst) }
      case CONNECT(m1, m2) => { checkMetro2(m1, lst) && checkMetro2(m2, lst) }
      case AREA(a, STATION(s)) => { searchList(s, a :: lst) }
      case AREA(a, AREA(b, m)) => { checkMetro2(AREA(b, m), a :: lst) }
      case AREA(a, CONNECT(m1, m2)) => {
        checkMetro2(m1, a :: lst) && checkMetro2(m2, a :: lst)
      }
    }
  }
    
  def checkMetro(met: Metro): Boolean = { checkMetro2(met, Nil()) }
}