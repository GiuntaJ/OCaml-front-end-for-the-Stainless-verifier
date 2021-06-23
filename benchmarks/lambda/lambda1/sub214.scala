import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub214 {
  /*2009-11718 박준상 2-1*/
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkArea(((name, lst))) = {
    lst match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == name) true else checkArea(name, tl) }
    }
  }
  
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def checkStation(lst, met) = {
        met match {
          case STATION(n) => { checkArea(n, lst) }
          case AREA(n, m) => { checkStation(n :: lst, m) }
          case CONNECT(m1, m2) => {
            checkStation(lst, m1) && checkStation(lst, m2)
          }
        }
      }
      checkStation(Nil(), metro)
    }
  }
}