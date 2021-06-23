import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub52 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def listcheck: (Var, List[Var]) => Boolean = {
    case (var0, varl) =>
      {
        varl match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (var0 == hd) true else listcheck(var0, tl) }
        }
    }
  }
  
  def lamfind: (Lambda, List[Var]) => Boolean = {
    case (lam, varl) =>
      {
        lam match {
          case V(v) => { listcheck(v, varl) }
          case P(v, lm) => {
            val _6 = {
              val vl = varl ++ List(v)
              val _7 = {
                val film = lamfind(lm, vl)
                film
              }
            }
          }
          case C(lm1, lm2) => {
            val _2 = {
              val film1 = lamfind(lm1, varl)
              val _3 = {
                val film2 = lamfind(lm2, varl)
                film1 && film2
              }
            }
          }
        }
    }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case V(v) => { false }
          case P(v, lm) => {
            val _10 = {
              val chlm = lamfind(lm, List(v))
              chlm
            }
          }
          case C(lm1, lm2) => { false }
        }
    }
  )
              
}
