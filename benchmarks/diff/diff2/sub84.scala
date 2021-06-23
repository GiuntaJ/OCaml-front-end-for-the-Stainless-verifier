import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub84 {
  
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
          case Const(a) => { Const(0) }
          case Var(v) => { if (v == var0) Const(1) else Var(v) }
          case Power(v, a) => {
            
              if (
                v == var0
              ) {
                a match {
                  case 2 => { Times(List(Const(2), Var(v))) }
                  case 1 => { Const(1) }
                  case 0 => { Const(0) }
                  case _ => { Times(List(Const(a), Power(v, a - 1))) }
                } 
              } else {
                Power(v, a)
              }
          }
          case Times(l) => {
            l match {
              case Cons(h, t) => {
                
                  if (
                    t == Nil()
                  ) {
                    diff(h, var0) 
                  } else {
                    Sum(
                      List(Times(diff(h, var0) :: t),
                       Times(List(h, diff(Times(t), var0)))))
                  }
              }
            }
          }
          case Sum(l) => {
            val _2 = {
              def subsum(l) = {
                l match {
                  case Nil() => { Nil() }
                  case Cons(h, t) => {
                    h match {
                      case Const(0) => { subsum(t) }
                      case Times(Cons(Const(0), Nil())) => { subsum(t) }
                      case _ => { diff(h, var0) :: subsum(t) }
                    }
                  }
                }
              }
              Sum(subsum(l))
            }
          }
        }
    }
  }
    
}