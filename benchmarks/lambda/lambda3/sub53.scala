import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub53 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  
  def is_there(x, arr) = {
    arr match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (x == hd) true else is_there(x, tl) }
    }
  }
  
  def find(lam, arr) = {
    lam match {
      case V(x) => { (is_there(x, arr), arr) }
      case P(x, l) => {
        val _6 = {
          val arr1 = x :: arr
          l match {
            case V(x1) => { find(V(x1), arr1) }
            case P(x1, l1) => { find(P(x1, l1), arr1) }
            case C(l1, l2) => {
              l1 match {
                case V(x2) => {
                  val _9 = {
                    val ((t1, a1)) = find(V(x2), arr1)
                    val _10 = {
                      val ((t2, a2)) = find(l2, a1)
                      (t1 && t2, a2)
                    }
                  }
                }
                case _ => { find(l, arr1) }
              }
            }
          }
        }
      }
      case C(l1, l2) => {
        val _2 = {
          val ((t1, a1)) = find(l1, arr)
          val _3 = {
            val ((t2, a2)) = find(l2, a1)
            (t1 && t2, a2)
          }
        }
      }
    }
  }
  /*(find l1 arr)&&(find l2 arr)*/
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        find(lam, Nil()) match {
          case (true, _) => { true }
          case (false, _) => { false }
        }
    }
  )
}