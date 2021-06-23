import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub130 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def checklambda: (List[Var], Lambda) => Boolean = {
    case (lst, lam) =>
      {
        lam match {
          case V(x) => { lst.contains(x) }
          case P(x, l) => { checklambda(x :: lst, l) }
          case C(l1, l2) => { checklambda(lst, l1) && checklambda(lst, l2) }
        }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { checklambda(Nil(), lam) } )
}