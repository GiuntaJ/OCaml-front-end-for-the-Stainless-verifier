import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub56 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def smrz: Aexp => Aexp = (
    (aexp) =>
      {
        aexp match {
          case Times(l) => {
            l match {
              case Cons(e, Nil()) => { e }
              case Cons(e, Cons(Times(l), t)) => { smrz(Times(e :: (l ++ t))) }
              case Cons(Const(a), Cons(Const(b), t)) => {
                smrz(Times(Const(a * b) :: t))
              }
              case Cons(Var(v), Cons(Var(v2), t)) => {
                
                  if (
                    v == v2
                  ) {
                    smrz(Times(Power(v, 2) :: t)) 
                  } else {
                    Times(Var(v) :: Var(v2) :: t)
                  }
              }
              case Cons(Var(v), Cons(Power(v2, i), t)) => {
                
                  if (
                    v == v2
                  ) {
                    smrz(Times(Power(v, i + 1) :: t)) 
                  } else {
                    Times(Var(v) :: Power(v2, i) :: t)
                  }
              }
              case Cons(Power(v2, i), Cons(Var(v), t)) => {
                
                  if (
                    v == v2
                  ) {
                    smrz(Times(Power(v, i + 1) :: t)) 
                  } else {
                    Times(Var(v) :: Power(v2, i) :: t)
                  }
              }
              case Cons(Power(v, i), t) => {
                
                  if (
                    i == 0
                  ) {
                    smrz(Times(t)) 
                  } else if (
                    i == 1
                  ) {
                    smrz(Times(Var(v) :: t)) 
                  } else {
                    Times(Power(v, i) :: t)
                  }
              }
              case Cons(Const(a), t) => {
                if (a == 0) Const(0) else Times(Const(a) :: t)
              }
              case Cons(h, t) => { Times(List(smrz(h), smrz(Times(t)))) }
              case _ => { Times(l) }
            }
          }
          case Sum(l) => {
            l match {
              case Cons(e, Nil()) => { e }
              case Cons(Const(0), t) => { smrz(Sum(t)) }
              case Cons(e, Cons(Sum(l), t)) => { smrz(Sum(e :: (l ++ t))) }
              case Cons(Const(a), Cons(Const(b), t)) => {
                smrz(Sum(Const(a + b) :: t))
              }
              case Cons(Var(v), Cons(Var(v2), t)) => {
                if (v == v2) smrz(Sum(Const(2) :: Var(v) :: t)) else Sum(l)
              }
              case Cons(h, t) => { Sum(List(smrz(h), smrz(Sum(t)))) }
              case _ => { Sum(l) }
            }
          }
          case _ => { aexp }
        }
    }
  )
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(i) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Var(v) }
          case Power(v, i) => {
            
              if (
                v == x
              ) {
                
                  if (
                    i == 2
                  ) {
                    Times(List(Const(2), Var(v))) 
                  } else if (
                    i == 1
                  ) {
                    Const(1) 
                  } else {
                    Times(List(Const(i), Power(v, i - 1)))
                  } 
              } else {
                Power(v, i)
              }
          }
          case Times(l) => {
            val _5 = {
              val ll = smrz(Times(l))
              ll match {
                case Times(Cons(Const(a), Cons(Var(v), t))) => {
                  
                    if (
                      v == x
                    ) {
                      smrz(Times(Const(a) :: t)) 
                    } else {
                      Times(Const(a) :: Var(v) :: t)
                    }
                }
                case Times(Cons(Const(a), Cons(Power(v, i), t))) => {
                  
                    if (
                      v == x
                    ) {
                      smrz(
                        Times(
                          List(Const(a),
                           smrz(Times(diff(smrz(Power(v, i)), x) :: t))))) 
                    } else {
                      Times(Const(a) :: Power(v, i) :: t)
                    }
                }
                case Times(Cons(e, Nil())) => { diff(e, x) }
                case _ => { ll }
              }
            }
          }
          case Sum(l) => {
            val _2 = {
              val ll = smrz(Sum(l))
              ll match {
                case Sum(Cons(e, Nil())) => { smrz(diff(e, x)) }
                case Sum(Cons(h, t)) => {
                  smrz(Sum(List(smrz(diff(h, x)), diff(Sum(t), x))))
                }
                case e => { smrz(diff(e, x)) }
              }
            }
          }
        }
    }
  }
}
