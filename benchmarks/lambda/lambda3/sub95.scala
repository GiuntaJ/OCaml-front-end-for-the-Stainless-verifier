import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub95 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val bindlst: List[A] = Nil()
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def bindchk: (Lambda, List[Var]) => List[Var] = {
            case (sublam, benv) =>
              {
                sublam match {
                  case V(x) => { benv }
                  case P(x, e) => { bindchk(e, x :: benv) }
                  case C(e1, e2) => { bindchk(e2, bindchk(e1, benv)) }
                }
            }
          }
          val _5 = {
            val bindlst = bindchk(lam, Nil())
            val _6 = {
              val lookup: (Var, List[Var]) => Boolean = {
                case (x, blst) =>
                  {
                    val _9 = {
                      def lookupproc: (Var, List[Var]) => Boolean = {
                        case (xx, lst) =>
                          {
                            lst match {
                              case Nil() => { false }
                              case Cons(hd, tl) => {
                                if (hd == xx) true else lookupproc(xx, tl)
                              }
                            }
                        }
                      }
                      lookupproc(x, blst)
                    }
                }
              }
              val _10 = {
                def isbind: (Lambda, List[Var]) => Boolean = {
                  case (lda, bindenv) =>
                    {
                      lda match {
                        case V(x) => { lookup(x, bindenv) }
                        case P(x, e) => { isbind(e, bindenv) }
                        case C(e1, e2) => {
                          
                            if (
                              isbind(e1, bindenv) == true &&
                              isbind(e2, bindenv) == true
                            ) {
                              true 
                            } else {
                              false
                            }
                        }
                      }
                  }
                }
                isbind(lam, bindlst)
              }
            }
          }
        }
    }
  )
}