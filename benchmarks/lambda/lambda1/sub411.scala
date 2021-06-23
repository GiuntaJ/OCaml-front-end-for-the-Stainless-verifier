import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub411 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def check(id) = {
    (
      x =>
        x match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == id) true else check(id, tl) }
        }
    )
  }
  
  def foo(areas) = {
    (
      x =>
        x match {
          case STATION(id) => { check(id, areas) }
          case AREA(id, m) => { foo(List(id) ++ areas, m) }
          case CONNECT(m1, m2) => { foo(areas, m1) && foo(areas, m2) }
        }
    )
  }
  
  def checkMetro: Metro => Boolean = foo(Nil())
  
  
}
