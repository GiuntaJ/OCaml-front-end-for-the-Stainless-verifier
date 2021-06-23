import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub72 {
  /* 4190.310 Programming Languages - Daegeun Lee <elnn@elnn.kr> */
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  val diff: (Ae, String) => Ae = {
    case (e, x) =>
      {
        val _2 = {
          def diff_0(((e, x))) = {
            e match {
              case CONST(c) => { CONST(0) }
              case VAR(y) => { if (x == y) CONST(1) else CONST(0) }
              case POWER(y, n) => {
                if (x == y) TIMES(List(CONST(n), POWER(x, n - 1))) else CONST(0)
              }
              case TIMES(Nil()) => { assert(false, "InvalidArgument") }
              case TIMES(Cons(hd, Nil())) => { diff_0(hd, x) }
              case TIMES(Cons(hd, tl)) => {
                SUM(
                  List(TIMES(diff_0(hd, x) :: tl),
                   TIMES(List(hd, diff_0(TIMES(tl), x)))))
              }
              case SUM(Nil()) => { assert(false, "InvalidArgument") }
              case SUM(Cons(hd, Nil())) => { diff_0(hd, x) }
              case SUM(Cons(hd, tl)) => {
                SUM(List(diff_0(hd, x), diff_0(SUM(tl), x)))
              }
            }
          }
          val _3 = {
            def simplify(e) = {
              e match {
                case CONST(c) => { e }
                case VAR(y) => { e }
                case POWER(y, 1) => { VAR(y) }
                case POWER(y, n) => { POWER(y, n) }
                case TIMES(Nil()) => { assert(false, "InvalidArgument") }
                case TIMES(Cons(hd, Nil())) => { simplify(hd) }
                case TIMES(Cons(hd, tl)) => {
                  val _10 = {
                    val t1 = simplify(hd)
                    val _11 = {
                      val t2 = simplify(TIMES(tl))
                      (t1, t2) match {
                        case (CONST(x), CONST(y)) => { CONST(x * y) }
                        case (CONST(0), _) => { CONST(0) }
                        case (_, CONST(0)) => { CONST(0) }
                        case (t1, CONST(1)) => { t1 }
                        case (CONST(1), t2) => { t2 }
                        case (t1, TIMES(t)) => { TIMES(t1 :: t) }
                        case (TIMES(t), t2) => { TIMES(t2 :: t) }
                        case (t1, t2) => { TIMES(List(t1, t2)) }
                      }
                    }
                  }
                }
                case SUM(Nil()) => { assert(false, "InvalidArgument") }
                case SUM(Cons(hd, Nil())) => { simplify(hd) }
                case SUM(Cons(hd, tl)) => {
                  val _6 = {
                    val t1 = simplify(hd)
                    val _7 = {
                      val t2 = simplify(SUM(tl))
                      (t1, t2) match {
                        case (CONST(x), CONST(y)) => { CONST(x + y) }
                        case (t1, CONST(0)) => { t1 }
                        case (CONST(0), t2) => { t2 }
                        case (t1, SUM(t)) => { SUM(t1 :: t) }
                        case (SUM(t), t2) => { SUM(t2 :: t) }
                        case (t1, t2) => { SUM(List(t1, t2)) }
                      }
                    }
                  }
                }
              }
            }
            simplify(diff_0(e, x))
          }
        }
    }
  }
}