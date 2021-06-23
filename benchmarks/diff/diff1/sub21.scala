import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub21 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff: (Ae, String) => Ae = {
    case (alexp, str) =>
      {
        val _2 = {
          val zeroTIMES: Ae => Ae = (
            (a) =>
              {
                a match {
                  case TIMES(Cons(hd, tl)) => {
                    if (hd == CONST(0) || tl.contains(CONST(0))) CONST(0) else a
                  }
                  case _ => { a }
                }
            }
          )
          alexp match {
            case CONST(c) => { CONST(0) }
            case VAR(s) => { if (s ne str) CONST(0) else CONST(1) }
            case POWER(s, n) => {
              
                if (
                  s ne str
                ) {
                  CONST(0) 
                } else if (
                  n == 0
                ) {
                  CONST(1) 
                } else if (
                  n == 1
                ) {
                  CONST(n) 
                } else if (
                  n == 2
                ) {
                  TIMES(List(CONST(n), VAR(s))) 
                } else {
                  TIMES(List(CONST(n), POWER(s, n - 1)))
                }
            }
            case TIMES(Cons(hd, tl)) => {
              
                if (
                  tl == Nil()
                ) {
                  diff(hd, str) 
                } else {
                  val _11 = {
                    val ht = zeroTIMES(TIMES(diff(hd, str) :: tl))
                    val _12 = {
                      val st = zeroTIMES(TIMES(List(hd, diff(TIMES(tl), str))))
                      
                        if (
                          ht == CONST(0) && st == CONST(0)
                        ) {
                          CONST(0) 
                        } else if (
                          ht == CONST(0)
                        ) {
                          st 
                        } else if (
                          st == CONST(0)
                        ) {
                          ht 
                        } else {
                          SUM(List(ht, st))
                        }
                    }
                  }
                }
            }
            case TIMES(Nil()) => { CONST(0) }
            case SUM(Cons(hd, tl)) => {
              
                if (
                  tl == Nil()
                ) {
                  diff(hd, str) 
                } else {
                  val _6 = {
                    val h = diff(hd, str)
                    val _7 = {
                      val t = diff(SUM(tl), str)
                      
                        if (
                          h == CONST(0)
                        ) {
                          t 
                        } else if (
                          t == CONST(0)
                        ) {
                          h 
                        } else {
                          SUM(List(h, t))
                        }
                    }
                  }
                }
            }
            case SUM(Nil()) => { CONST(0) }
          }
        }
    }
  }
}