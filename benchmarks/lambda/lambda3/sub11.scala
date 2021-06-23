import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub11 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type LamEnv = List[(Var, Lambda)]
  /* initailze env */
  val empty_lamEnv: List[A] = Nil()
  def extend_lamEnv(((x, v)), e) = { ((x, v)) :: e }
  def lookup_lamEnv[A, B](x: A, e: List[(A, B)]): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(((y, v)), tl) => { if (x == y) true else lookup_lamEnv(x, tl) }
    }
  }
  
  
  def check_inter: (Lambda, List[(Var, Lambda)]) => Boolean = {
    case (lam, lamEnv) =>
      {
        lam match {
          case P(v, l) => {
            val _6 = {
              val lamEnv_0 = extend_lamEnv(v, l, lamEnv)
              check_inter(l, lamEnv_0)
            }
          }
          case C(l1, l2) => {
            val _2 = {
              val o1 = check_inter(l1, lamEnv)
              val _3 = {
                val o2 = check_inter(l2, lamEnv)
                o1 && o2
              }
            }
          }
          case V(v) => { lookup_lamEnv(v, lamEnv) }
        }
    }
  }
  
  
  def check: Lambda => Boolean = ( (lam) => { check_inter(lam, empty_lamEnv) } )
}