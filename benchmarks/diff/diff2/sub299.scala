import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub299 {
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
          case Const(integer) => { Const(0) }
          case Var(str) => { if (str == x) Const(1) else Var(str) }
          case Power(str, integer) => {
            
              if (
                str != x
              ) {
                Power(str, integer) 
              } else {
                Times(List(Const(integer)) ++ List(Power(str, integer - 1)))
              }
          }
          case Times(list1) => {
            list1 match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(diff(hd, x) :: tl),
                   Times(List(hd, diff(Times(tl), x)))))
              }
            }
          }
          case Sum(list2) => {
            list2 match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
          
          
  diff(Times(List(Const(2), Var("x"), Const(2), Var("x"))), "x")
  
  /*
  type aexp =
    | Const of int
    | Var of string
    | Power of string * int
    | Times of aexp list
    | Sum of aexp list
  
  let rec diff : aexp * string -> aexp
  = fun (exp, x) -> exp /*TODO*/;;
  
  */
}