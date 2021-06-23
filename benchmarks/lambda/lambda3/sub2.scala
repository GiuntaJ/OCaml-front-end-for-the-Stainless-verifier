import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub2 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def search_var_list: (List[Var], Var) => Boolean = {
    case (vl, v) =>
      {
        vl match {
          case Cons(hd, tl) => { if (hd == v) true else search_var_list(tl, v) }
          case _ => { false }
        }
    }
  }
  
  def check_rec: (Lambda, List[Var]) => Boolean = {
    case (lam, vl) =>
      {
        lam match {
          case V(v) => { if (search_var_list(vl, v)) true else false }
          case P(v, l) => {
            val _2 = {
              val new_vl = v :: vl
              check_rec(l, new_vl)
            }
          }
          case C(l1, l2) => { check_rec(l1, vl) && check_rec(l2, vl) }
        }
    }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _7 = {
          val var_list = Nil()
          check_rec(lam, var_list)
        }
    }
  )
}