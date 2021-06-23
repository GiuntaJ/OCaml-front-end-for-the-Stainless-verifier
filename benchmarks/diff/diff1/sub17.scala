import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub17 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def remove_nth[A](l: List[A], n: Int63): List[A] = {
    val _2 = {
      def remove(l, n, c) = {
        if (c == n) l.tail else l.head :: remove(l.tail, n, c + 1)
      }
      remove(l, n, 0)
    }
  }
  
  def diff(((ae, str))) = {
    val _5 = {
      def diff_times(((l, v))) = {
        val _8 = {
          def diff_times_0(((l, v, cur))) = {
            
              if (
                cur > l.length - 1
              ) {
                Nil() 
              } else {
                TIMES(diff(l.apply(cur), v) :: remove_nth(l, cur)) ::
                diff_times_0(l, v, cur + 1)
              }
          }
          SUM(diff_times_0(l, v, 0))
        }
      }
      val _9 = {
        def diff_sum(((l, v))) = {
          val _12 = {
            def ds(((l, v))) = {
              l match {
                case Nil() => { Nil() }
                case Cons(hd, tl) => { diff(hd, v) :: ds(tl, v) }
              }
            }
            SUM(ds(l, v))
          }
        }
        ae match {
          case CONST(i) => { CONST(0) }
          case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
          case POWER(s, i) => {
            if (s == str) TIMES(List(CONST(i), POWER(s, i - 1))) else CONST(0)
          }
          case TIMES(l) => { diff_times(l, str) }
          case SUM(l) => { diff_sum(l, str) }
        }
      }
    }
  }
  
  val ae1: Ae = TIMES(List(VAR("a"), POWER("x", 2)))
  val ae2: Ae = TIMES(List(VAR("b"), VAR("x")))
  val ae3: Ae = VAR("c")
  val f1: Ae = SUM(List(ae1, ae2, ae3))
}