import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub245 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
    
    
  def reverse: List[A] => List[A] = (
    (lst) =>
      {
        lst match {
          case Nil() => { lst }
          case Cons(hd, tl) => { reverse(tl) ++ List(hd) }
        }
    }
  )
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(a) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Var(a) }
          case Power(a, b) => {
            
              if (
                a == x
              ) {
                b match {
                  case 2 => { Times(List(Const(2), Var(a))) }
                  case _ => { Times(List(Const(b), Power(a, b - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Times(l) => {
            l match {
              case Nil() => { Times(l) }
              case _ => {
                val _2 = {
                  val newList = reverse(l)
                  newList match {
                    case Nil() => { Times(l) }
                    case Cons(hd, tl) => { Times(diff(hd, x) :: tl) }
                  }
                }
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { Sum(l) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
  
  /*
  I implemented a list reversing function because essentially we want to use the second element of the list part of
  [Times l]. It's easier to reverse the list and perform the [diff] function than to try to extract that element.
  */
  
  
  
}
