import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub298 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
    
  def consume_times: List[Aexp] => List[Aexp] = (
    (l) =>
      {
        val _4 = {
          def inner(l, const) = {
            l match {
              case Cons(Const(n), tl) => { inner(tl, n * const) }
              case Cons(hd, tl) => { hd :: inner(tl, const) }
              case Nil() => { List(Const(const)) }
            }
          }
          inner(l, 1)
        }
    }
  )
  
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        val _7 = {
          def diff_sum(l) = {
            l match {
              case Cons(hd, tl) => { diff(hd, x) :: diff_sum(tl) }
              case Nil() => { Nil() }
            }
          }
          val _8 = {
            def diff_times(l) = {
              l match {
                case Cons(Times(t), tl) => { diff_times(t ++ tl) }
                case Cons(Const(n), tl) => { Const(n) :: diff_times(tl) }
                case Cons(Power(v, n), tl) => {
                  
                    if (
                      x == v
                    ) {
                      
                        if (
                          n == 1
                        ) {
                          diff_times(tl) 
                        } else if (
                          n == 2
                        ) {
                          Const(n) :: Var(v) :: diff_times(tl) 
                        } else {
                          Const(n) :: Power(v, n - 1) :: diff_times(tl)
                        } 
                    } else {
                      Power(v, n) :: diff_times(tl)
                    }
                }
                case Cons(Var(v), tl) => {
                  
                    if (
                      x == v
                    ) {
                      Const(1) :: diff_times(tl) 
                    } else {
                      Var(v) :: diff_times(tl)
                    }
                }
                case Cons(hd, tl) => { hd :: diff_times(tl) }
                case Nil() => { Nil() }
              }
            }
            exp match {
              case Const(_) => { Const(0) }
              case Var(v) => { if (x == v) Const(1) else Const(0) }
              case Power(v, num) => {
                
                  if (
                    x == v
                  ) {
                    
                      if (
                        num == 1
                      ) {
                        Const(1) 
                      } else if (
                        num == 2
                      ) {
                        Times(List(Const(num), Var(v))) 
                      } else {
                        Times(List(Const(num), Power(v, num - 1)))
                      } 
                  } else {
                    Const(0)
                  }
              }
              case Times(l) => { Times(consume_times(diff_times(l))) }
              case Sum(l) => { Sum(diff_sum(l)) }
            }
          }
        }
    }
  }
     
     
}