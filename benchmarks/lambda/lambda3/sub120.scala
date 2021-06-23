import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub120 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def check1(lambda, myvarlst) = {
            lambda match {
              case V(v) => { myvarlst }
              case P(v, l) => { check1(l, v :: myvarlst) }
              case C(l1, l2) => { check1(l1, myvarlst) ++ check1(l1, Nil()) }
            }
          }
          val _5 = {
            def check2(lambda, myfreevarlst) = {
              lambda match {
                case V(v) => { v :: myfreevarlst }
                case P(v, l) => { check2(l, myfreevarlst) }
                case C(l1, l2) => {
                  check2(l1, myfreevarlst) ++ check2(l2, Nil())
                }
              }
            }
            val _6 = {
              def check3(varlst, freevarlst) = {
                freevarlst match {
                  case Nil() => { true }
                  case Cons(var0, tl) => {
                    varlst match {
                      case Nil() => { false }
                      case Cons(var2, tl2) => {
                        
                          if (
                            var0 == var2
                          ) {
                            check3(tl, freevarlst) 
                          } else {
                            check3(varlst, tl2)
                          }
                      }
                    }
                  }
                }
              }
              val _7 = {
                val varlst = check1(lam, Nil())
                val freevarlst = check2(lam, Nil())
                check3(varlst, freevarlst)
              }
            }
          }
        }
    }
  )
}