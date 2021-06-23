import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub278 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(_) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Const(0) }
          case Power(v, n) => {
            
              if (
                v == x
              ) {
                
                  if (
                    n == 0
                  ) {
                    Const(0) 
                  } else if (
                    n == 1
                  ) {
                    Const(1) 
                  } else if (
                    n == 2
                  ) {
                    Times(List(Const(n), Var(v))) 
                  } else {
                    Times(List(Const(n), Power(v, n - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(xs) => {
            val _5 = {
              def bind(s, l) = {
                s match {
                  case Cons(t, ts) => { bind(ts, t :: l) }
                  case Nil() => { l }
                }
              }
              val _6 = {
                def times(s, lst) = {
                  val _9 = {
                    def group(lst) = {
                      lst match {
                        case Cons(e, Nil()) => { e }
                        case _ => { Times(lst) }
                      }
                    }
                    lst match {
                      case Cons(e, es) => {
                        diff(e, x) match {
                          case Const(0) => { times(e :: s, es) }
                          case Const(1) => {
                            group(bind(s, es)) :: times(e :: s, es)
                          }
                          case e_0 => {
                            group(bind(s, e_0 :: es)) :: times(e :: s, es)
                          }
                        }
                      }
                      case Nil() => { Nil() }
                    }
                  }
                }
                times(Nil(), xs) match {
                  case Nil() => { Const(0) }
                  case Cons(e, Nil()) => { e }
                  case terms => { Sum(terms) }
                }
              }
            }
          }
          case Sum(xs) => {
            val _2 = {
              def sum(lst) = {
                lst match {
                  case Cons(e, es) => {
                    diff(e, x) match {
                      case Const(0) => { sum(es) }
                      case e_0 => { e_0 :: sum(es) }
                    }
                  }
                  case Nil() => { Nil() }
                }
              }
              sum(xs) match {
                case Nil() => { Const(0) }
                case Cons(e, Nil()) => { e }
                case terms => { Sum(terms) }
              }
            }
          }
        }
    }
  }
  
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}