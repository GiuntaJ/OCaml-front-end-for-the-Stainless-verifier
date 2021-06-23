import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub134 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}		
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(t) => { if (t == var0) Const(1) else Const(0) }
          case Power(t, n) => {
            if (t == var0) Times(List(Const(n), Power(t, n - 1))) else Const(0)
          }
          case Times(lst) => { Times(timediff(lst, var0)) }
          case Sum(lst) => { Sum(sumdiff(lst, var0)) }
        }
    }
  }
  def timediff: (List[Aexp], String) => List[Aexp] = {
    case (lst, var0) =>
      {
        lst match {
          case Nil() => { List(Const(1)) }
          case Cons(hd, tl) => {
            (hd match {
               case Const(n) => { Const(n) }
               case Var(t) => { if (t == var0) Const(1) else Const(0) }
               case Power(t, n) => {
                 
                   if (
                     t == var0
                   ) {
                     Times(List(Const(n), Power(t, n - 1))) 
                   } else {
                     Const(0)
                   }
               }
               case Times(l2) => { Times(timediff(l2, var0)) }
               case Sum(l2) => { Sum(sumdiff(l2, var0)) }
             }) ::
            timediff(tl, var0)
          }
        }
    }
  }
  def sumdiff: (List[Aexp], String) => List[Aexp] = {
    case (lst, var0) =>
      {
        lst match {
          case Nil() => { List(Const(0)) }
          case Cons(hd, tl) => {
            (hd match {
               case Const(n) => { Const(0) }
               case Var(t) => { if (t == var0) Const(1) else Const(0) }
               case Power(t, n) => {
                 
                   if (
                     t == var0
                   ) {
                     Times(List(Const(n), Power(t, n - 1))) 
                   } else {
                     Const(0)
                   }
               }
               case Times(l2) => { Times(timediff(l2, var0)) }
               case Sum(l2) => { Sum(sumdiff(l2, var0)) }
             }) ::
            sumdiff(tl, var0)
          }
        }
    }
  }
}