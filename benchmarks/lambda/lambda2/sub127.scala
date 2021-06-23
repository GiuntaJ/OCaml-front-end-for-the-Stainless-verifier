import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub127 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def removeMatch: (List[Var], Var) => List[Var] = {
            case (l, v) =>
              {
                l match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => {
                    
                      if (
                        hd == v
                      ) {
                        removeMatch(tl, v) 
                      } else {
                        hd :: removeMatch(tl, v)
                      }
                  }
                }
            }
          }
          val _5 = {
            def eval: Exp => List[Var] = (
              (exp) =>
                {
                  exp match {
                    case V(v) => { List(v) }
                    case P(v, e) => { removeMatch(eval(e), v) }
                    case C(e1, e2) => { eval(e1) ++ eval(e2) }
                  }
              }
            )
            if (eval(exp) == Nil()) true else false
          }
        }
    }
  ) 
}