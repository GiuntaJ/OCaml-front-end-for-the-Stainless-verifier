import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub268 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
    
  /*let rec listToelement : 'a list -> 'a list -> 'a list
  = fun l1 l2 -> */ /*l1은 [Const 1]로 시작*/
   /*)match l2 with
      | [] -> l1
      | hd::tl -> (l1 @ [hd]) @ listToelement l1 tl;;*/
  
      
  val listToelement2: List[A] => Aexp = (
    (lst) =>
      {
        lst match {
          case Cons(hd, tl) => {
            hd match {
              case Times(l) => { Times(l) }
              case Sum(l) => { Sum(l) }
              case Const(x) => { Const(x) }
              case Var(x) => { Var(x) }
              case Power(s, n) => { Power(s, n) }
            }
          }
        }
    }
  )
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(n) => { Const(0) }
          case Var(s) => { if (s == x) Const(1) else Var(s) }
          case Power(s, n) => {
            
              if (
                s == x && n > 2
              ) {
                Times(List(Const(n), Power(s, n - 1))) 
              } else if (
                s == x && n == 2
              ) {
                Times(List(Const(n), Var(x))) 
              } else {
                Power(s, n)
              }
          }
          case Times(l) => { diff_Times(l, x) }
          case Sum(l) => { diff_Sum(l, x) }
        }
    }
  }
  def diff_Times: (List[Aexp], String) => Aexp = {
    case (l, x) =>
      {
        l match {
          case Cons(hd, tl) => {
            Sum(
              List(Times(List(diff(hd, x), listToelement2(tl))),
               Times(List(hd, diff(listToelement2(tl), x)))))
          }
        }
    }
  }
  def diff_Sum: (List[Aexp], String) => Aexp = {
    case (l, x) =>
      {
        l match {
          case Cons(hd, tl) => {
            Sum(List(diff(hd, x), diff(listToelement2(tl), x)))
          }
        }
    }
  }
  
  
      /*| Times l -> match l with
                    | hd::tl -> Sum ((Times (diff (hd, x) :: tl)) @ (hd :: Times (diff (Times tl, x)))) 
      | Sum l -> match l with
      
                  |hd::tl -> Sum (diff(hd, x) :: diff(tl, x));;*/
   
   
   
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
  
  
}
