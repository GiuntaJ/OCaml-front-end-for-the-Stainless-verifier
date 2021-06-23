import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub66 {
  /*
   * Programming Languages, 2013 Fall.
   * HW Code for Exercise 2-2
   * Department of Computer Science and Engineering
   * 2006-11855, Jung Yonghyuk (ever103@snu.ac.kr)
   */
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  /* diff: ae * string -> ae */
  def diff(((ae, s))) = {
    ae match {
      case CONST(i) => { CONST(0) }
      case VAR(s_0) => { if (s == s_0) CONST(1) else CONST(0) }
      case POWER(s_0, i) => {
        if (s == s_0) TIMES(List(CONST(i), POWER(s, i - 1))) else CONST(0)
      }
      case SUM(al) => {
        al match {
          case Nil() => { assert(false, "InvalidArgument") }
          case _ => { SUM(al.map(( (x) => { diff(x, s) } ))) }
        }
      }
      case TIMES(al) => {
        al match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(h, Nil()) => { diff(h, s) }
          case Cons(h, t) => {
            val _2 = {
              val f = h
              val _3 = {
                val f_0 = diff(f, s)
                val _4 = {
                  val g = TIMES(t)
                  val _5 = {
                    val g_0 = diff(g, s)
                    SUM(List(TIMES(List(f_0, g)), TIMES(List(f, g_0))))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}