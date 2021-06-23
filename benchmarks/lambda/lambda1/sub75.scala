import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub75 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  sealed case class NotArea() extends Exception {}
  
  def checkMetro(x: Metro): Boolean = {
    val _2 = {
      def deleteElem(a, lst) = {
        lst match {
          case Nil() => { Nil() }
          case Cons(hd, Nil()) => { if (a == hd) Nil() else List(hd) }
          case Cons(hd, tl) => {
            if (a == hd) deleteElem(a, tl) else hd :: deleteElem(a, tl)
          }
        }
      }
      val _3 = {
        def mkstrlst(t) = {
          t match {
            case STATION(a) => { List(a) }
            case AREA(a, b) => { deleteElem(a, mkstrlst(b)) }
            case CONNECT(a, b) => { mkstrlst(a) ++ mkstrlst(b) }
          }
        }
        if (mkstrlst(x) == Nil()) true else false
      }
    }
  }
}