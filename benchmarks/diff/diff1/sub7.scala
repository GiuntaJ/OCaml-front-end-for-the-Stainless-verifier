import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub7 {
  /* diff: ae * string -> ae */
  sealed case class Error(param0: String) extends Exception {}
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def remove[A](x: A, lst: List[A]): List[A] = {
    lst match {
      case Nil() => { Nil() }
      case Cons(h, t) => { if (h == x) t else h :: remove(x, t) }
    }
  }
  
  def remove_zero(exp: Ae): Ae = {
    exp match {
      case TIMES(lst) => { if (lst.contains(CONST(0))) CONST(0) else TIMES(lst)
      }
      case _ => { exp }
    }
  }
  
  
  def pretty(ae: Ae): Ae = {
    ae match {
      case TIMES(lst) => {
        
          if (
            lst.map(pretty).contains(CONST(0))
          ) {
            CONST(0) 
          } else {
            val _6 = {
              val remove1 = List.fold_right(
                {
                  case (a, b) => { if (a == CONST(1)) b else a :: b }
                },
                lst, Nil())
              val _7 = {
                val remove_times = List.fold_right(
                  {
                    case (a, b) =>
                      {
                        a match {
                          case TIMES(lst) => { lst ++ b }
                          case _ => { a :: b }
                        }
                    }
                  },
                  remove1, Nil())
                val _8 = {
                  val final0 = remove_times.map(pretty)
                  if (final0.length == 1) final0.head else TIMES(final0)
                }
              }
            }
          }
      }
      case SUM(lst) => {
        val _2 = {
          val remove0 = List.fold_right(
            {
              case (a, b) =>
                { if (pretty(a) == CONST(0)) b else pretty(a) :: b
              }
            },
            lst, Nil())
          
            if (
              remove0 == Nil()
            ) {
              CONST(0) 
            } else if (
              remove0.length == 1
            ) {
              remove0.head 
            } else {
              SUM(remove0)
            }
        }
      }
      case POWER(a, b) => { if (b == 0) CONST(1) else ae }
      case _ => { ae }
    }
  }
  
  def diff(((exp, x))) = {
    exp match {
      case CONST(_) => { CONST(0) }
      case VAR(y) => { if (y == x) CONST(1) else CONST(0) }
      case POWER(y, i) => {
        
          if (
            y == x
          ) {
            if (i == 1) CONST(1) else TIMES(List(CONST(i), POWER(x, i - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(lst) => {
        pretty(
          SUM(
            lst.map(( (y) => { remove_zero(TIMES(List(diff(y, x)) ++ remove(y, lst))) } ))))
      }
      case SUM(lst2) => { pretty(SUM(lst2.map(( (y) => { diff(y, x) } )))) }
    }
  }
}
