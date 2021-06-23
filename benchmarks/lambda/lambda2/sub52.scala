import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub52 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  val check: Exp => Boolean = (
    (ex) =>
      {
        val _4 = {
          def var_list: Exp => List[Var] = (
            (ex2) =>
              {
                ex2 match {
                  case V(v) => { List(v) }
                  case P(v2, ex3) => { var_list(ex3) }
                  case C(ex3, ex4) => { var_list(ex3) ++ var_list(ex4) }
                }
            }
          )
          val _5 = {
            def proc_list: Exp => List[Var] = (
              (ex2) =>
                {
                  ex2 match {
                    case V(v) => { Nil() }
                    case P(v2, ex3) => { v2 :: proc_list(ex3) }
                    case C(ex3, ex4) => { proc_list(ex3) ++ proc_list(ex4) }
                  }
              }
            )
            val _6 = {
              def _in_: (List[Var], Var) => Boolean = {
                case (vlist, v) =>
                  {
                    vlist match {
                      case Nil() => { false }
                      case Cons(hd, tl) => { if (hd == v) true else _in_(tl, v)
                      }
                    }
                }
              }
              val _7 = {
                def check2: (List[Var], List[Var]) => Boolean = {
                  case (vlist, plist) =>
                    {
                      vlist match {
                        case Nil() => { true }
                        case Cons(hd, tl) => {
                          _in_(plist, hd) && check2(tl, plist)
                        }
                      }
                  }
                }
                check2(var_list(ex), proc_list(ex))
              }
            }
          }
        }
    }
  )
}
