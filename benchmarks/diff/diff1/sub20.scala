import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub20 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  sealed case class CriticalE() extends Exception {}
  def diff(((a, str))) = {
    a match {
      case CONST(s) => { CONST(0) }
      case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
      case POWER(s, n) => {
        if (s == str) TIMES(List(CONST(n), POWER(s, n - 1))) else CONST(0)
      }
      case TIMES(Cons(hd, Nil())) => { diff(hd, str) }
      case TIMES(Cons(hd, tl)) => {
        val _9 = {
          val primehd = diff(hd, str)
          val _10 = {
            val primetl = diff(TIMES(tl), str)
            
              if (
                primehd == CONST(0)
              ) {
                
                  if (
                    primetl == CONST(0)
                  ) {
                    CONST(0) 
                  } else {
                    TIMES(List(hd, diff(TIMES(tl), str)))
                  } 
              } else if (
                primetl == CONST(0)
              ) {
                TIMES(primehd :: tl) 
              } else {
                SUM(List(TIMES(primehd :: tl), TIMES(List(hd, primetl))))
              }
          }
        }
      }
      case TIMES(Nil()) => { CONST(0) }
      case SUM(lst) => {
        val _2 = {
          def summod(l) = {
            l match {
              case Nil() => { Nil() }
              case Cons(hd, tl) => {
                val _5 = {
                  val primehd = diff(hd, str)
                  if (primehd == CONST(0)) summod(tl) else primehd :: summod(tl)
                }
              }
            }
          }
          val _6 = {
            val result = summod(lst)
            if (result == Nil()) CONST(0) else SUM(result)
          }
        }
      }
    }
  }
  
}
