import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub476 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def isin: (Name, List[Name]) => Boolean = {
    case (id, nlist) =>
      {
        nlist match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == id) true else isin(id, tl) }
        }
    }
  }
  
  def checkList: (Metro, List[Name]) => Boolean = {
    case (m, nlist) =>
      {
        m match {
          case STATION(id) => { isin(id, nlist) }
          case AREA(id, m_) => { checkList(m_, id :: nlist) }
          case CONNECT(m1, m2) => { checkList(m1, nlist) && checkList(m2, nlist)
          }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = ( (m) => { checkList(m, Nil()) } )
}