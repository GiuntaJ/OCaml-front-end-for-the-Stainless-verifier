import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub99 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((input, x))) = {
    input match {
      case CONST(c) => { CONST(0) }
      case VAR(var0) => { if (var0 == x) CONST(1) else CONST(0) }
      case POWER(var0, n) => {
        
          if (
            n eq 0
          ) {
            diff(CONST(1), x) 
          } else if (
            var0 == x
          ) {
            TIMES(List(CONST(n), POWER(var0, n - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(l) => {
        val _5 = {
          def time(al) = {
            al match {
              case Nil() => { assert(false, "InvalidArgument") }
              case Cons(hd, Nil()) => { diff(hd, x) }
              case Cons(hd, tl) => {
                SUM(
                  List(TIMES(List(diff(hd, x)) ++ tl)) ++
                  List(TIMES(List(hd) ++ List(diff(TIMES(tl), x)))))
              }
            }
          }
          time(l)
        }
      }
      case SUM(l) => {
        val _2 = {
          def sum(l) = {
            l match {
              case Nil() => { assert(false, "InvalidArgument") }
              case Cons(h, Nil()) => { diff(h, x) }
              case Cons(h, t) => {
                SUM(List(diff(h, x)) ++ List(diff(SUM(t), x)))
              }
            }
          }
          sum(l)
        }
      }
    }
  }
}
