import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub269 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        val _2 = {
          val cleanTimes: Aexp => Aexp = (
            (exps) =>
              {
                exps match {
                  case Times(xs) => {
                    
                      if (
                        xs.length ==
                          xs.filter(( (x) => { x != Const(0) } )).length
                      ) {
                        
                          if (
                            xs.filter(( (x) => { x != Const(1) } )).length ==
                              1
                          ) {
                            xs.filter(( (x) => { x != Const(1) } )).head 
                          } else {
                            Times(xs.filter(( (x) => { x != Const(1) } )))
                          } 
                      } else {
                        Const(0)
                      }
                  }
                  case _ => { exps }
                }
            }
          )
          val _3 = {
            val cleanPower: Aexp => Aexp = (
              (exps) =>
                {
                  exps match {
                    case Power(a, n) => { if (n == 1) Var(a) else exps }
                    case _ => { exps }
                  }
              }
            )
            val _4 = {
              val cleanSum: Aexp => Aexp = (
                (exps) =>
                  {
                    exps match {
                      case Sum(xs) => { if (xs.length == 1) xs.head else exps }
                      case _ => { exps }
                    }
                }
              )
              exp match {
                case Const(n) => { Const(0) }
                case Sum(xs) => {
                  cleanSum(
                    Sum(
                      xs.map(( (a) => { diff(a, x) } )).filter(( (x) => { x != Const(0) } ))))
                }
                case Var(a) => { if (a == x) Const(1) else Const(0) }
                case Power(a, n) => {
                  
                    if (
                      a == x
                    ) {
                      cleanTimes(
                        Times(List(Const(n), cleanPower(Power(a, n - 1))))) 
                    } else {
                      Const(0)
                    }
                }
                case Times(xs) => {
                  cleanSum(
                    Sum(
                      xs.map((
                        (a) =>
                          {
                            cleanTimes(
                              Times(
                                xs.map(( (b) => { if (b == a) diff(b, x) else b } ))))
                        }
                      )).filter(( (x) => { x != Const(0) } ))))
                }
              }
            }
          }
        }
    }
  }
}