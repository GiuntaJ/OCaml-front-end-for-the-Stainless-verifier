import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub133 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  	
  	def diffRec: (List[Aexp], String) => List[Aexp] = {
    case (aexpList, var0) =>
      {
        aexpList match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => { diff(hd, var0) :: diffRec(tl, var0) }
        }
    }
  }
  def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Sum(aexpList) => { Sum(diffRec(aexpList, var0)) }
          case Power(string1, int1) => {
            
              if (
                string1 == var0
              ) {
                
                  if (
                    int1 == 0
                  ) {
                    Const(0) 
                  } else if (
                    int1 == 1
                  ) {
                    Const(1) 
                  } else {
                    Times(List(Const(int1), Power(var0, int1 - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(aexpList) => {
            aexpList match {
              case Nil() => { Sum(Nil()) }
              case Cons(hd, tl) => {
                
                  if (
                    tl == Nil()
                  ) {
                    diff(hd, var0) 
                  } else {
                    Sum(
                      List(Times(diff(hd, var0) :: tl),
                       Times(List(hd, diff(Times(tl), var0)))))
                  }
              }
            }
          }
          case Var(string1) => { diff(Power(string1, 1), var0) }
          case Const(int1) => { Const(0) }
        }
    }
  } 						
}