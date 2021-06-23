import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub37 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
  def check: Exp => Boolean = (
    (e) =>
      {
        val _10 = {
          val a = eval(e, Nil())
          a match {
            case true => { true }
            case false => { false }
          }
        }
    }
  )
  def eval: (Exp, List[Var]) => Boolean = {
    case (e, env) =>
      {
        e match {
          case V(x) => { lookup(x, env) }
          case P(x, e1) => {
            val _5 = {
              val v = lookup(x, env)
              v match {
                case false => { eval(e1, extend(x, env)) }
                case true => { eval(e1, env) }
              }
            }
          }
          case C(e1, e2) => {
            val _2 = {
              val a = eval(e1, env)
              a match {
                case false => { false }
                case true => { eval(e2, env) }
              }
            }
          }
        }
    }
  }
  def lookup: (Var, List[Var]) => Boolean = {
    case (x, env) =>
      {
        env match {
          case Nil() => { false }
          case Cons(v, tl) => { if (x == v) true else lookup(x, tl) }
        }
    }
  }
  def extend: (Var, List[Var]) => List[Var] = {
    case (x, env) => { x :: env }
  }
}