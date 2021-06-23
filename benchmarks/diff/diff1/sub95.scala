import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub95 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((aexpr, x))) = {
    aexpr match {
      case CONST(y) => { CONST(0) }
      case VAR(y) => { if (x == y) CONST(1) else CONST(0) }
      case POWER(y, n) => {
        if (x == y) TIMES(List(CONST(n), POWER(y, n - 1))) else CONST(0)
      }
      case TIMES(l) => {
        val _8 = {
          val len = l.length
          
            if (
              len == 0
            ) {
              assert(false, "InvalidArgument") 
            } else {
              val _10 = {
                val hd = l.head
                val _11 = {
                  val tl = l.tail
                  
                    if (
                      len == 1
                    ) {
                      diff(hd, x) 
                    } else {
                      SUM(
                        List(TIMES(List(diff(TIMES(List(hd)), x)) ++(tl)),
                         TIMES(List(hd, diff(TIMES(tl), x)))))
                    }
                }
              }
            }
        }
      }
      case SUM(l) => {
        val _2 = {
          val len = l.length
          
            if (
              len == 0
            ) {
              assert(false, "InvalidArgument") 
            } else {
              val _4 = {
                val hd = l.head
                val _5 = {
                  val tl = l.tail
                  
                    if (
                      len == 1
                    ) {
                      diff(hd, x) 
                    } else {
                      SUM(List(diff(hd, x), diff(SUM(tl), x)))
                    }
                }
              }
            }
        }
      }
    }
  }
}