import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub138 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  /*=====================================================*/
  /*하나짜리 처리*/
  /*=====================================================*/
  def diffMono: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(con) => { Const(0) }
          case Var(str) => { if (str == var0) Const(1) else Const(0) }
          case Power(str, integer) => {
            
              if (
                str == var0
              ) {
                
                  if (
                    integer == 1
                  ) {
                    Const(integer) 
                  } else if (
                    integer == 2
                  ) {
                    Times(List(Const(integer), Var(str))) 
                  } else {
                    Times(List(Const(integer), Power(str, integer - 1)))
                  } 
              } else {
                Const(0)
              }
          }
        }
    }
  }
  def dT: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Times(Cons(head, tail)) => {
            
              if (
                tail.length == 0
              ) {
                dT(head, var0) 
              } else {
                Sum(
                  List(Times(dT(head, var0) :: tail),
                   Times(List(head, dT(Times(tail), var0)))))
              }
          }
          case Sum(Cons(head, tail)) => {
            
              if (
                tail.length == 0
              ) {
                dT(head, var0) 
              } else {
                Sum(List(dT(head, var0), dT(Sum(tail), var0)))
              }
          }
          case _ => { diffMono(exp, var0) }
        }
    }
  }
  
   
   
  
  /*=====================================================*/
  /*시작!*/
  /*=====================================================*/
  val diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Times(mlist) => { dT(exp, var0) }
          case Sum(mlist) => { dT(exp, var0) }
          case _ => { diffMono(exp, var0) }
        }
    }
  }
}