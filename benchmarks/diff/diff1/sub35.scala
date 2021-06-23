import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub35 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  			
  def diff(((ae, s))) = {
    val _2 = {
      def replace_nth(l, n, c) = {
        l match {
          case Cons(h, t) => {
            if (n == 0) c :: t else h :: replace_nth(t, n - 1, c)
          }
          case Nil() => { Nil() }
        }
      }
      ae match {
        case CONST(_) => { CONST(0) }
        case VAR(s_0) => { if (s == s_0) CONST(1) else CONST(0) }
        case POWER(s_0, i) => {
          
            if (
              s == s_0
            ) {
              if (i == 0) CONST(0) else TIMES(List(CONST(i), POWER(s, i - 1))) 
            } else {
              CONST(0)
            }
        }
        case TIMES(l) => {
          val _5 = {
            def func(n, l, l_0) = {
              
                if (
                  n == l.length
                ) {
                  l_0 
                } else {
                  func(
                    n + 1, l,
                    l_0 ++ List(TIMES(replace_nth(l, n, diff(l.apply(n), s)))))
                }
            }
            val _6 = {
              val l_0 = func(0, l, Nil())
              SUM(l_0)
            }
          }
        }
        case SUM(l) => { SUM(l.map(( (x) => { diff(x, s) } ))) }
      }
    }
  }
  
  /* TEST SET */
  /*
  let x = "x";;
  let y = "y";;
  diff (CONST 1, x);;
  diff (VAR x, x);;
  diff (POWER (x, -1), x);;
  diff (TIMES [(VAR x); (VAR x)], x);;
  diff (SUM [(VAR x); (VAR y); TIMES [(VAR x); (VAR x)]], x);;
  diff (SUM [(VAR y); (TIMES [(VAR x); (VAR y); (VAR y)])], y);;
  */
}