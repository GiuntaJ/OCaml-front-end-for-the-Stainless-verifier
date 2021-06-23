import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub158 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def simp: Aexp => Aexp = (
    (exp) =>
      {
        exp match {
          case Const(n) => { Const(n) }
          case Var(x) => { Var(x) }
          case Power(s, i) => {
            i match {
              case 0 => { Const(1) }
              case 1 => { Var(s) }
              case _ => { Power(s, i) }
            }
          }
          case Times(expli) => {
            expli match {
              case Nil() => { Const(1) }
              case Cons(h, Nil()) => { simp(h) }
              case Cons(h, t) => {
                h match {
                  case Const(0) => { Const(0) }
                  case Const(1) => { Times(t) }
                  case _ => { Times(h :: t) }
                }
              }
            }
          }
          case Sum(expli) => {
            expli match {
              case Nil() => { Const(0) }
              case Cons(h, Nil()) => { simp(h) }
              case Cons(h, t) => {
                h match {
                  case Const(0) => { Sum(t) }
                  case _ => { Sum(h :: t) }
                }
              }
            }
          }
        }
    }
  )
                /* h: aexp, t: aexp list */
  
   
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(x) => { if (x == var0) Const(1) else Const(0) }
          case Power(s, n) => {
            
              if (
                s == var0
              ) {
                n match {
                  case 0 => { Const(0) }
                  case 1 => { Const(1) }
                  case _ => { Times(List(Const(n), Power(s, n - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Times(li) => {
            li match {
              case Nil() => { Const(1) }
              case Cons(h, Nil()) => { diff(h, var0) }
              case Cons(h, t) => {
                h match {
                  case Const(n) => {
                    simp(Times(List(Const(n), diff(Times(t), var0))))
                  }
                  case _ => {
                    simp(
                      Sum(
                        List(Times(diff(h, var0) :: t),
                         Times(List(h, diff(Times(t), var0))))))
                  }
                }
              }
            }
          }
          case Sum(li) => {
            li match {
              case Nil() => { Const(0) }
              case Cons(h, t) => {
                simp(Sum(List(diff(h, var0), diff(Sum(t), var0))))
              }
            }
          }
        }
    }
  }
}