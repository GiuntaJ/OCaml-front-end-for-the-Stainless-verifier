import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub198 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def com(((l, n))) = {
    l match {
      case Nil() => { false }
      case Cons(head, tail) => { head == n || com(tail, n) }
    }
  }
  def cMetro(((l, m))) = {
    m match {
      case STATION(n) => { com(l, n) }
      case AREA(n, mm) => { cMetro(n :: l, mm) }
      case CONNECT(m1, m2) => { cMetro(l, m1) && cMetro(l, m2) }
    }
  }
  def checkMetro(m) = { cMetro(Nil(), m) }
}