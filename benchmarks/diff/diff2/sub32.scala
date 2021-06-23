import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub32 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
   
  
  def diff(((e, v))) = {
    e match {
      case Const(n) => { Const(0) }
      case Var(x) => { if (x == v) Const(1) else Var(x) }
      case Power(x, i) => {
        if (i eq 1) Const(1) else Times(List(Const(i), Power(x, i - 1)))
      }
      case Times(al) => {
        val _11 = {
          def diff_times(el, x) = {
            el match {
              case Nil() => { Nil() }
              case Cons(Const(c), tl) => { Const(c) :: diff_times(tl, x) }
              case Cons(Times(ts), tl) => { diff_times(ts ++(tl), x) }
              case Cons(hd, tl) => { diff(hd, v) :: diff_times(tl, x) }
            }
          }
          val _12 = {
            def clean_times(rs, coef) = {
              rs match {
                case Nil() => { (Nil(), coef) }
                case Cons(Const(n), tl) => { clean_times(tl, coef * n) }
                case Cons(hd, tl) => {
                  val _15 = {
                    val ((t0, m)) = clean_times(tl, coef)
                    (hd :: t0, m)
                  }
                }
              }
            }
            val _16 = {
              def concat_times(ts) = {
                ts match {
                  case Nil() => { Nil() }
                  case Cons(Times(t1), tl) => { concat_times(t1 ++(tl)) }
                  case Cons(hd, tl) => { hd :: concat_times(tl) }
                }
              }
              val _17 = {
                val t1 = diff_times(al, v)
                val _18 = {
                  val t2 = concat_times(t1)
                  val _19 = {
                    val ((t3, n)) = clean_times(t2, 1)
                    if (n == 1) Times(t3) else Times(Const(n) :: t3)
                  }
                }
              }
            }
          }
        }
      }
      case Sum(al) => {
        val _2 = {
          def diff_list(el, x) = {
            el match {
              case Nil() => { Nil() }
              case Cons(hd, tl) => { diff(hd, x) :: diff_list(tl, x) }
            }
          }
          val _3 = {
            def clean_sum(rs, con) = {
              rs match {
                case Nil() => { (Nil(), con) }
                case Cons(Times(Cons(a, Nil())), tl) => {
                  clean_sum(a :: tl, con)
                }
                case Cons(Const(n), tl) => { clean_sum(tl, con + n) }
                case Cons(hd, tl) => {
                  val _6 = {
                    val ((ts, con1)) = clean_sum(tl, con)
                    (hd :: ts, con1)
                  }
                }
              }
            }
            val _7 = {
              val s1 = diff_list(al, v)
              val _8 = {
                val ((s2, n)) = clean_sum(s1, 0)
                if (n == 0) Sum(s2) else Sum(s2 ++(List(Const(n))))
              }
            }
          }
        }
      }
    }
  }
}