import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub13 {
  /* HW2 exercise2 2009-11697 Kim HyunJoon */
  /* Mathemadiga */
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff: (Ae, String) => Ae = {
    case (exp, str) =>
      {
        val _2 = {
          def partialDiff(lst, n) = {
            lst match {
              case Nil() => { Nil() }
              case Cons(hd, tl) => {
                if (n == 0) myDiff(hd) :: tl else hd :: partialDiff(tl, n - 1)
              }
            }
          }
          def myTimes(lst, nth) = {
            lst match {
              case Nil() => { Nil() }
              case _ => {
                
                  if (
                    nth < lst.length
                  ) {
                    TIMES(partialDiff(lst, nth)) :: myTimes(lst, nth + 1) 
                  } else {
                    Nil()
                  }
              }
            }
          }
          def myDiff(exp) = {
            exp match {
              case CONST(n) => { CONST(0) }
              case VAR(var0) => { if (var0 == str) CONST(1) else CONST(0) }
              case POWER(var0, n) => {
                
                  if (
                    var0 == str
                  ) {
                    
                      if (
                        n == 0
                      ) {
                        CONST(0) 
                      } else {
                        TIMES(List(CONST(n), POWER(var0, n - 1)))
                      } 
                  } else {
                    CONST(0)
                  }
              }
              case TIMES(lst) => { SUM(myTimes(lst, 0)) }
              case SUM(lst) => { SUM(lst.map(myDiff)) }
            }
          }
          val _3 = {
            def removeZero(exp) = {
              exp match {
                case TIMES(lst) => {
                  
                    if (
                      lst.contains(CONST(0))
                    ) {
                      CONST(0) 
                    } else {
                      TIMES(lst.map(removeZero))
                    }
                }
                case SUM(lst) => {
                  SUM(
                    lst.filter(( (exp) => { if (exp ne CONST(0)) true else false } )).map(removeZero))
                }
                case _ => { exp }
              }
            }
            removeZero(myDiff(exp))
          }
        }
    }
  }
  
  
  /*
  let one = CONST 1
  let x = VAR "x"
  let x2 = POWER ("x", 2)
  let a = VAR "a"
  let b = VAR "b"
  let c = VAR "c"
  let eq = SUM [(TIMES [a;x2]);(TIMES [b;x]);c]
  */
}
