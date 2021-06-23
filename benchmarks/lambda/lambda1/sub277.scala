import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub277 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def is_has: (List[Name], Name) => Boolean = {
    case (l, n) =>
      {
        
          if (
            l.length eq 0
          ) {
            false 
          } else if (
            l.head == n
          ) {
            true 
          } else {
            is_has(l.tail, n)
          }
    }
  }
  
  def check: (Metro, List[Name]) => Boolean = {
    case (m, l) =>
      {
        m match {
          case STATION(_n) => { is_has(l, _n) }
          case AREA(_n, _m) => { check(_m, _n :: l) }
          case CONNECT(_m1, _m2) => { check(_m1, l) && check(_m2, l) }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = ( (m) => { check(m, Nil()) } )
}