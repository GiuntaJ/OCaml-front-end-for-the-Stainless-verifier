import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub56 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def comp(va, exorg) = {
    exorg match {
      case V(va1) => { false }
      case P(va1, ex1) => { if (va == va1) true else comp(va, ex1) }
      case C(ex1, ex2) => { comp(va, ex1) || comp(va, ex2) }
    }
  }
  
    def find(ex, exorg) = {
    ex match {
      case V(va1) => { comp(va1, exorg) }
      case P(va1, ex1) => { find(ex1, exorg) }
      case C(ex1, ex2) => { find(ex1, exorg) && find(ex2, exorg) }
    }
  }
  
    def check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case V(va) => { false }
          case P(va, ex) => { find(ex, e) }
          case C(ex1, ex2) => { check(ex1) && check(ex2) }
        }
    }
  )
}