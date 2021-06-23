import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub10 {
  
   /*********************/ 
   /* Problem 2 */ 
   /*********************/ 
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String 
  /* TODO */ 
  def vcheck: (Lambda, List[Var]) => Boolean = {
    case (lam, clist) =>
      {
        lam match {
          case V(var0) => {
            clist match {
              case Cons(hd, tl) => {
                if (var0 == hd) true else vcheck(V(var0), tl)
              }
              case Nil() => { false }
            }
          }
          case P(var0, lam1) => { vcheck(lam1, var0 :: clist) }
          case C(lam1, lam2) => { vcheck(lam1, clist) && vcheck(lam2, clist) }
        }
    }
  }
  
  
  def check: Lambda => Boolean = ( (lam) => { vcheck(lam, Nil()) } )
}
