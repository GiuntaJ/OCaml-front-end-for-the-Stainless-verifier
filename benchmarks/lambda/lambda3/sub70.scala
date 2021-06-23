import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub70 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def get(lam, lst1, lst2) = {
            lam match {
              case V(x) => { (lst1, x :: lst2) }
              case P(x, e) => {
                val _10 = {
                  val lst_0 = x :: lst1
                  get(e, lst_0, lst2)
                }
              }
              case C(e1, e2) => {
                val _7 = {
                  val t = get(e1, lst1, lst2)
                  get(e2, fst(t), snd(t))
                }
              }
            }
          }
          val _11 = {
            def search(lst1, lst2) = {
              lst2 match {
                case Nil() => { true }
                case Cons(hd, tl) => {
                  val _14 = {
                    def find(p, lst) = {
                      lst match {
                        case Nil() => { false }
                        case Cons(hd, tl) => {
                          if (hd == p) true else find(p, tl)
                        }
                      }
                    }
                    if (find(hd, lst1)) search(lst1, tl) else false
                  }
                }
              }
            }
            lam match {
              case V(x) => { true }
              case _ => {
                val _17 = {
                  val t = get(lam, Nil(), Nil())
                  search(fst(t), snd(t))
                }
              }
            }
          }
        }
    }
  )
}