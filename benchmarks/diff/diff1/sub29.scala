import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub29 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def minimize(ae: Ae): Ae = {
    val _2 = {
      def sum_trim(al) = {
        al match {
          case Nil() => { Nil() }
          case Cons(SUM(lst), tl) => { sum_trim(lst ++ tl) }
          case Cons(hd, tl) => { hd :: sum_trim(tl) }
        }
      }
      val _3 = {
        def mul_trim(al) = {
          al match {
            case Nil() => { Nil() }
            case Cons(TIMES(lst), tl) => { mul_trim(lst ++ tl) }
            case Cons(hd, tl) => { hd :: mul_trim(tl) }
          }
        }
        val _4 = {
          def iter(lst) = {
            lst match {
              case Nil() => { Nil() }
              case Cons(hd, tl) => { minimize(hd) :: iter(tl) }
            }
          }
          ae match {
            case SUM(lst) => { SUM(iter(sum_trim(lst))) }
            case TIMES(lst) => { TIMES(iter(mul_trim(lst))) }
            case x => { x }
          }
        }
      }
    }
  }
  
  
  def diff(((alex, str))) = {
    val _7 = {
      def sum(al) = {
        al match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => { diff(hd, str) :: sum(tl) }
        }
      }
      val _8 = {
        def mul(al, lst) = {
          al match {
            case Nil() => { Nil() }
            case Cons(hd, tl) => {
              TIMES(diff(hd, str) :: (tl ++ lst)) :: mul(tl, hd :: lst)
            }
          }
        }
        alex match {
          case CONST(i) => { CONST(0) }
          case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
          case POWER(s, i) => {
            
              if (
                s == str
              ) {
                
                  if (
                    i == 1
                  ) {
                    CONST(1) 
                  } else {
                    TIMES(List(CONST(i), POWER(s, i - 1)))
                  } 
              } else {
                CONST(0)
              }
          }
          case TIMES(al) => { minimize(SUM(mul(al, Nil()))) }
          case SUM(al) => { minimize(SUM(sum(al))) }
        }
      }
    }
  }
}