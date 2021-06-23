import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub39 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def test(lam) = {
    lam match {
      case V(x) => { List(("v", x)) }
      case P(x, l) => {
        val _6 = {
          val lst = test(l)
          List(("p", x)) ++ lst
        }
      }
      case C(l1, l2) => {
        val _2 = {
          val lst1 = test(l1)
          val _3 = {
            val lst2 = test(l2)
            lst1 ++ lst2
          }
        }
      }
    }
  }
  
  def find(num, lst) = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        if (hd == num) find(num, tl) else List(hd) ++ find(num, tl)
      }
    }
  }
  
  def remove(lst) = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        hd match {
          case (p, x) => {
            
              if (
                p == "p"
              ) {
                val _7 = {
                  val lst1 = find("v", x, tl)
                  remove(lst1)
                } 
              } else if (
                p == "v"
              ) {
                hd :: remove(tl) 
              } else {
                assert(false, "Failure with a")
              }
          }
        }
      }
    }
  }
  
  def order(lst) = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        hd match {
          case (p, x) => { if (p == "p") order(tl) else hd :: order(tl) }
        }
      }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _12 = {
          val lst1 = test(lam)
          val _13 = {
            val lst2 = remove(lst1)
            val _14 = {
              val lst3 = order(lst2)
              lst3 match {
                case Nil() => { true }
                case _ => { false }
              }
            }
          }
        }
    }
  )
}