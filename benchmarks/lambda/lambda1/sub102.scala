import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub102 {
  /*2009-11718 1-8*/
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkStation(((m, lst))) = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        if (hd == m) checkStation(m, tl) else hd :: checkStation(m, tl)
      }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def isInArea(met, lst) = {
        met match {
          case STATION(name) => { name :: lst }
          case AREA(name, mtro) => { checkStation(name, isInArea(mtro, lst)) }
          case CONNECT(met1, met2) => {
            isInArea(met1, lst) ++ isInArea(met2, lst)
          }
        }
      }
      if (isInArea(metro, Nil()) == Nil()) true else false
    }
  }
}
