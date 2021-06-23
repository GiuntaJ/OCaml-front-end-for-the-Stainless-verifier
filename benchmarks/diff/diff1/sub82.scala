import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub82 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  
  
  def diff(((e, var0))) = {
    e match {
      case CONST(n) => { CONST(0) }
      case VAR(s) => { if (s == var0) CONST(1) else CONST(0) }
      case POWER(s, n) => {
        
          if (
            n eq 0
          ) {
            CONST(0) 
          } else if (
            s == var0
          ) {
            TIMES(List(CONST(n), POWER(s, n - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(l) => {
        val _5 = {
          def mkarr(lst, var0, ind, cnt) = {
            
              if (
                cnt < lst.length
              ) {
                
                  if (
                    ind eq cnt
                  ) {
                    List(diff(lst.apply(cnt), var0)) ++
                    mkarr(lst, var0, ind, cnt + 1) 
                  } else {
                    List(lst.apply(cnt)) ++ mkarr(lst, var0, ind, cnt + 1)
                  } 
              } else {
                Nil()
              }
          }
          val _6 = {
            def fnc(ind) = { mkarr(l, var0, ind, 0) }
            val _7 = {
              def implmapi(fnc, l, cnt) = {
                
                  if (
                    cnt < l.length
                  ) {
                    List(TIMES(fnc(cnt))) ++ implmapi(fnc, l, cnt + 1) 
                  } else {
                    Nil()
                  }
              }
              SUM(implmapi(fnc, l, 0))
            }
          }
        }
      }
      case SUM(l) => {
        val _2 = {
          def fnc = (
            x =>
              x match {
                case Nil() => { Nil() }
                case Cons(x, l) => { insert(diff(x, var0), fnc(l)) }
              }
          )
          def insert(ele) = {
            (
              x =>
                x match {
                  case Nil() => { List(ele) }
                  case Cons(x, l) => { x :: (l ++ List(ele)) }
                }
            )
          }
          SUM(fnc(l))
        }
      }
    }
  }
}