import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub462 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /*station list?*/
  def checkArray(((n, arr1, arr2))) = {
    arr2 match {
      case Nil() => { arr1 }
      case Cons(h, t) => {
        if (n == h) checkArray(n, arr1, t) else checkArray(n, h :: arr1, t)
      }
    }
  }
  def checkStation(m: Metro): List[Name] = {
    m match {
      case STATION(n) => { List(n) }
      case AREA(n, m1) => {
        val _2 = {
          val arr = checkStation(m1)
          checkArray(n, Nil(), arr)
        }
      }
      case CONNECT(m1, m2) => { checkStation(m1) ++ checkStation(m2) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    val _5 = {
      val result = checkStation(m)
      if (result == Nil()) true else false
    }
  }
}