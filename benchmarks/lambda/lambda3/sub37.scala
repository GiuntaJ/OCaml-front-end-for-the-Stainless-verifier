import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub37 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Env2 = List[Var]
  
  def findVar: (Var, Env2) => Boolean = {
    case (x, env2) =>
      {
        env2 match {
          case Cons(hd, tl) => { if (hd == x) true else findVar(x, tl) }
          case Nil() => { false }
        }
    }
  }
    
  
  def test: (Lambda, Env2) => Boolean = {
    case (lam, env2) =>
      {
        lam match {
          case V(x) => { findVar(x, env2) }
          case P(x, l) => {
            val _2 = {
              val newEnv2 = x :: env2
              test(l, newEnv2)
            }
          }
          case C(l1, l2) => { test(l1, env2) && test(l2, env2) }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { test(lam, Nil()) } )
}