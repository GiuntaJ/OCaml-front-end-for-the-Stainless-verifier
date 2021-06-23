import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub78 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {} /* if argument of TIMES and SUM is an empty list */
  
  def diff: (Ae, String) => Ae = {
    case (ae, str) =>
      {
        ae match {
          case CONST(i) => { CONST(0) }
          case VAR(i) => { if (i == str) CONST(1) else CONST(0) }
          case POWER(i1, i2) => {
            
              if (
                i1 == str
              ) {
                
                  if (
                    i2 == 1
                  ) {
                    CONST(1) 
                  } else if (
                    i2 == 0
                  ) {
                    CONST(0) 
                  } else {
                    TIMES(List(CONST(i2), POWER(i1, i2 - 1)))
                  } 
              } else {
                CONST(0)
              }
          }
          case TIMES(i) => {
            i match {
              case Nil() => { assert(false, "InvalidArgument") }
              case Cons(h, t) => {
                
                  if (
                    t == Nil()
                  ) {
                    diff(h, str) 
                  } else {
                    SUM(
                      List(TIMES(List(diff(h, str)) ++ t),
                       TIMES(List(h, diff(TIMES(t), str)))))
                  }
              }
            }
          }
          case SUM(i) => {
            i match {
              case Nil() => { assert(false, "InvalidArgument") }
              case Cons(h, t) => {
                
                  if (
                    t == Nil()
                  ) {
                    diff(h, str) 
                  } else {
                    SUM(List(diff(h, str), diff(SUM(t), str)))
                  }
              }
            }
          }
        }
    }
  }
}