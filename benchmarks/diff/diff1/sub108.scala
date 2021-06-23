import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub108 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((f, v))) = {
    val _2 = {
      def doDiff(((f, v))) = {
        f match {
          case CONST(_) => { CONST(0) }
          case VAR(x) => { if (x == v) CONST(1) else CONST(0) }
          case POWER(x, p) => {
            
              if (
                p == 0
              ) {
                CONST(0) 
              } else if (
                x == v
              ) {
                TIMES(List(CONST(p), POWER(x, p - 1))) 
              } else {
                CONST(0)
              }
          }
          case TIMES(x) => {
            x match {
              case Cons(a, Nil()) => { doDiff(a, v) }
              case Cons(a, b) => {
                val _5 = {
                  val da = doDiff(a, v)
                  val _6 = {
                    val db = doDiff(TIMES(b), v)
                    SUM(List(TIMES(List(a, db)), TIMES(da :: b)))
                  }
                }
              }
              case Nil() => { assert(false, "InvalidArgument") }
            }
          }
          case SUM(x) => {
            x match {
              case Cons(a, Nil()) => { doDiff(a, v) }
              case Cons(a, b) => { SUM(x.map(( (x) => { doDiff(x, v) } ))) }
              case Nil() => { assert(false, "InvalidArgument") }
            }
          }
        }
      }
      doDiff(f, v)
    }
  } 
    
}