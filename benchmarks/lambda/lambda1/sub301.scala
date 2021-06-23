import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub301 {
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def isContain(st: Name, l: List[Name]): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (st == hd) true else isContain(st, tl) }
    }
  }            
  		
  
  
  def subCheckMetro(m: Metro, l: List[Name]): Boolean = {
    m match {
      case STATION(n) => { isContain(n, l) }
      case AREA(nm, mtr) => { subCheckMetro(mtr, List(nm) ++(l)) }
      case CONNECT(m1, m2) => { subCheckMetro(m1, l) && subCheckMetro(m2, l) }
    }
  } 
  
  
  
  def checkMetro(m: Metro): Boolean = { subCheckMetro(m, Nil()) }
  	
}