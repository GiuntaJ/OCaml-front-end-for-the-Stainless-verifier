import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub93 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def ch: (Exp, List[Exp]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case V(a) => {
            lst match {
              case Nil() => { false }
              case Cons(hd, tl) => {
                
                  if (
                    hd == V(a)
                  ) {
                    true 
                  } else if (
                    tl == Nil()
                  ) {
                    false 
                  } else {
                    ch(V(a), tl)
                  }
              }
            }
          }
          case P(a, b) => { ch(b, V(a) :: lst) }
          case C(a, b) => { ch(a, lst) && ch(b, lst) }
        }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { ch(exp, Nil()) } )
}