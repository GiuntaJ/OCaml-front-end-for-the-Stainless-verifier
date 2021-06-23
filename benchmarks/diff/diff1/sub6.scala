import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub6 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  		
  def diff(((ae, str))) = {
    val _2 = {
      def isThereStr(ae, str) = {
        ae match {
          case CONST(i) => { false }
          case VAR(s) => { s == str }
          case POWER(s, i) => { s == str }
          case TIMES(l) => {
            l match {
              case Nil() => { false }
              case Cons(h, t) => {
                isThereStr(h, str) || isThereStr(TIMES(t), str)
              }
            }
          }
          case SUM(l) => {
            l match {
              case Nil() => { false }
              case Cons(h, t) => { isThereStr(h, str) || isThereStr(SUM(t), str)
              }
            }
          }
        }
      }
      val _3 = {
        def mul(al, str) = {
          al match {
            case Nil() => { Nil() }
            case Cons(h, Nil()) => {
              if (isThereStr(h, str)) List(diff(h, str)) else List(h)
            }
            case Cons(h, t) => {
              
                if (
                  isThereStr(h, str)
                ) {
                  List(diff(h, str)) ++(mul(t, str)) 
                } else {
                  List(h) ++(mul(t, str))
                }
            }
          }
        }
        ae match {
          case CONST(i) => { CONST(0) }
          case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
          case POWER(s, i) => {
            
              if (
                s == str
              ) {
                if (i > 1) TIMES(List(CONST(i), POWER(s, i - 1))) else CONST(1) 
              } else {
                CONST(0)
              }
          }
          case TIMES(l) => {
            if (isThereStr(ae, str)) TIMES(mul(l, str)) else CONST(0)
          }
          case SUM(l) => {
            l match {
              case Nil() => { CONST(0) }
              case Cons(h, Nil()) => { diff(h, str) }
              case Cons(h, t) => {
                SUM(List(diff(h, str)) ++(List(diff(SUM(t), str))))
              }
            }
          }
        }
      }
    }
  }
}