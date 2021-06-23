import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub31 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  
  def diff(((a, s))) = {
    val _2 = {
      def timesf(l, n) = {
        val _5 = {
          def timesfn(l, n) = {
            val _8 = {
              def cutlistf(l, n) = {
                if (n == 0) Nil() else l.head :: cutlistf(l.tail, n - 1)
              }
              val _9 = {
                def cutlistl(l, n) = {
                  if (n == 0) l.tail else cutlistl(l.tail, n - 1)
                }
                TIMES(
                  cutlistf(l, n) ++ List(diff(l.apply(n), s)) ++ cutlistl(l, n))
              }
            }
          }
          if (n == l.length) Nil() else timesfn(l, n) :: timesf(l, n + 1)
        }
      }
      val _10 = {
        def diffs(na) = { diff(na, s) }
        a match {
          case CONST(n) => { CONST(0) }
          case VAR(ms) => { if (ms == s) CONST(1) else CONST(0) }
          case POWER(ms, n) => {
            if (ms == s) TIMES(List(CONST(n), POWER(ms, n - 1))) else CONST(0)
          }
          case TIMES(l) => { SUM(timesf(l, 0)) }
          case SUM(l) => { SUM(l.map(diffs)) }
        }
      }
    }
  }
  	
}