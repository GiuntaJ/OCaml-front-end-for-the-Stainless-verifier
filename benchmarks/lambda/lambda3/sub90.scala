import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub90 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def bound_check(v, blist) = {
    blist match {
      case Nil() => { false }
      case Cons(h, t) => { if (h == v) true else bound_check(v, t) }
    }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def check_c(lam, blist) = {
            lam match {
              case V(v) => { bound_check(v, blist) }
              case P(v, l) => { check_c(l, v :: blist) }
              case C(a, b) => {
                if (check_c(a, blist)) check_c(b, blist) else false
              }
            }
          }
          check_c(lam, Nil())
        }
    }
  )
    
  
  /*let test1 = P ("a", V "a");;
  let test2 = P ("a", P ("a", V "a"));;
  let test3 = P ("a", P ("b", C (V "a", V "b")));;
  let test4 = P ("a", C (V "a", P ("b", V "a")));;
  
  let test5 = P ("a", V "b");;
  let test6 = P ("a", C (V "a", P ("b", V "c")));;
  let test7 = P ("a", P ("b", C (V "a", V "c")));;*/
}