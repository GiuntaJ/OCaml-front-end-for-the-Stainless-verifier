import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub202 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def exists_var: (Aexp, String) => Boolean = {
    case (f, x) =>
      {
        f match {
          case Sum(Nil()) => { false }
          case Times(Nil()) => { false }
          case Sum(Cons(hd, tl)) => {
            if (exists_var(hd, x) || exists_var(Sum(tl), x)) true else false
          }
          case Var(a) => { if (a == x) true else false }
          case Power(a, n) => { if (a == x) true else false }
          case Times(Cons(hd, tl)) => {
            if (exists_var(hd, x) || exists_var(Times(tl), x)) true else false
          }
          case Const(n) => { false }
        }
    }
  }
  
  def find_element: (Aexp, String) => Aexp = {
    case (f, x) =>
      {
        f match {
          case Sum(Nil()) => { Const(0) }
          case Times(Nil()) => { Const(0) }
          case Sum(Cons(hd, tl)) => {
            
              if (
                exists_var(hd, x)
              ) {
                Sum(List(find_element(hd, x), find_element(Sum(tl), x))) 
              } else {
                find_element(Sum(tl), x)
              }
          }
          case Var(a) => { if (a == x) Var(a) else Const(0) }
          case Power(a, n) => { if (a == x) Power(x, n) else Const(0) }
          case Times(lst) => {
            if (exists_var(Times(lst), x)) Times(lst) else Const(0)
          }
          case Const(_) => { Const(0) }
        }
    }
  }
  
  def differ: (Aexp, String) => Aexp = {
    case (f, x) =>
      {
        f match {
          case Sum(Nil()) => { Const(0) }
          case Times(Nil()) => { Const(1) }
          case Var(a) => { if (a == x) Const(1) else Var(a) }
          case Power(a, n) => {
            
              if (
                a == x
              ) {
                
                  if (
                    n == 1
                  ) {
                    Const(1) 
                  } else {
                    Times(List(Const(n), Power(a, n - 1)))
                  } 
              } else {
                Power(a, n)
              }
          }
          case Times(Cons(hd, tl)) => {
            Times(List(differ(hd, x), differ(Times(tl), x)))
          }
          case Sum(Cons(hd, tl)) => {
            Sum(List(differ(hd, x), differ(Sum(tl), x)))
          }
          case Const(a) => { Const(a) }
        }
    }
  }
  
  val diff: (Aexp, String) => Aexp = {
    case (e, x) => { differ(find_element(e, x), x) }
  }
}