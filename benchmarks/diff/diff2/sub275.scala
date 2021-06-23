import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub275 {
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
          case Const(c) => { Const(0) }
          case Var(m) => { if (m == x) Const(1) else Var(m) }
          case Power(m, c) => {
            
              if (
                m == x
              ) {
                c match {
                  case 0 => { Const(1) }
                  case _ => { Times(List(Const(c), Power(m, c - 1))) }
                } 
              } else {
                Power(m, c)
              }
          }
          case Times(m) => {
            m match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => { Times(List(hd, diff(Times(tl), x))) }
            }
          }
          case Sum(m) => {
            m match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
            
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}