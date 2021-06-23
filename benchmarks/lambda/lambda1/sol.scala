import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sol {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def is_mem: (List[Name], Name) => Boolean = {
    case (names, name) =>
      {
        names match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == name) true else is_mem(tl, name) }
        }
    }
  }
  
  def sub_checkMetro: (Metro, List[Name]) => Boolean = {
    case (met, names) =>
      {
        met match {
          case STATION(n) => { is_mem(names, n) }
          case AREA(n, m) => { sub_checkMetro(m, n :: names) }
          case CONNECT(m1, m2) => {
            sub_checkMetro(m1, names) && sub_checkMetro(m2, names)
          }
        }
    }
  }
  
  def checkMetro: Metro => Boolean = ( (met) => { sub_checkMetro(met, Nil()) } )
}