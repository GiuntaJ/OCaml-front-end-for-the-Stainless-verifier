import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub387 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (met) =>
      {
        met match {
          case STATION(n) => { false }
          case AREA(name, met) => { checkArea(met, List(name)) }
          case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
        }
    }
  )
  def checkArea: (Metro, List[String]) => Boolean = {
    case (m, nl) =>
      {
        (m, nl) match {
          case (STATION(name), Nil()) => { false }
          case (STATION(name), Cons(n, nl)) => {
            if (n == name) true else checkArea(STATION(name), nl)
          }
          case (CONNECT(m1, m2), nl) => { checkArea(m1, nl) && checkArea(m2, nl)
          }
          case (AREA(name, met), nl) => { checkArea(met, name :: nl) }
        }
    }
  }
}