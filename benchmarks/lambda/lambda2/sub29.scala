import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub29 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def ptree: (Var, Exp) => Boolean = {
    case (e1, e2) =>
      {
        e2 match {
          case P(x, y) => {
            y match {
              case P(a, b) => { ptree(a, b) || ptree(x, b) || ptree(e1, b) }
              case C(a, b) => {
                (ptree(x, a) || ptree(x, b)) && (ptree(e1, a) || ptree(e1, b))
              }
              case V(a) => { ptree(x, y) || ptree(e1, y) }
            }
          }
          case C(x, y) => { ptree(e1, x) && ptree(e1, y) }
          case V(x) => { e1 == x }
        }
    }
  }
    
    def ctree: (Exp, Exp) => Boolean = {
    case (e1, e2) =>
      {
        (e1, e2) match {
          case (P(x1, y1), P(x2, y2)) => { ptree(x1, y1) && ptree(x2, y2) }
          case (C(x1, y1), P(x2, y2)) => { ctree(x1, y1) && ptree(x2, y2) }
          case (P(x1, y1), C(x2, y2)) => { ptree(x1, y1) && ctree(x2, y2) }
          case (C(x1, y1), C(x2, y2)) => { ctree(x1, y1) && ctree(x2, y2) }
          case (V(x), _) => { false }
          case (_, V(y)) => { false }
        }
    }
  }
    
    val check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case P(x, y) => { ptree(x, y) }
          case C(x, y) => { ctree(x, y) }
          case V(x) => { true }
        }
    }
  )
}