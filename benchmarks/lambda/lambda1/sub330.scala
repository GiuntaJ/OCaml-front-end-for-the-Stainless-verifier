import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub330 {
  type Name = String
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def contains: (List[A], A) => Boolean = {
    case (l, item) =>
      {
        l match {
          case Nil() => { false }
          case Cons(h, tail) => { if (item == h) true else contains(tail, item)
          }
        }
    }
  }
  
  def metro: (List[Name], Metro) => Boolean = {
    case (l, m) =>
      {
        m match {
          case STATION(n) => { contains(l, n) }
          case AREA(id, area) => { metro(id :: l, area) }
          case CONNECT(m1, m2) => { metro(l, m1) && metro(l, m2) }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = ( (m) => { metro(Nil(), m) } )
}
