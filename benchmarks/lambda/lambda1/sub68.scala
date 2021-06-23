import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub68 {
  /*2009-11718 1-7*/
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkStation(((a, lst))) = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        if (a == hd) checkStation(a, tl) else hd :: checkStation(a, tl)
      }
    }
  }
  
  	
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def makeMetro(met, lst) = {
        met match {
          case STATION(a) => { a :: lst }
          case AREA(a, mtro) => { checkStation(a, makeMetro(mtro, lst)) }
          case CONNECT(met1, met2) => {
            makeMetro(met1, lst) ++ makeMetro(met2, lst)
          }
        }
      }
      if (makeMetro(met, Nil()) == Nil()) true else false
    }
  }
}
