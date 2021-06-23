import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub333 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def findName: (Name, List[Name]) => Boolean = {
    case (x, y) =>
      {
        y match {
          case Nil() => { false }
          case Cons(y_h, y_t) => { if (y_h == x) true else findName(x, y_t) }
        }
    }
  }
  
  def checkMetroList: (Metro, List[Name]) => Boolean = {
    case (x, y) =>
      {
        x match {
          case STATION(n) => { findName(n, y) }
          case AREA(n, m) => { checkMetroList(m, n :: y) }
          case CONNECT(m1, m2) => {
            checkMetroList(m1, y) && checkMetroList(m2, y)
          }
        }
    }
  }
   
  val checkMetro: Metro => Boolean = ( (x) => { checkMetroList(x, Nil()) } )
}