import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub114 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          val myvarlst = Nil()
          val myfreevarlst = Nil()
          val _5 = {
            def check2(lambda) = {
              lambda match {
                case V(v) => { myfreevarlst ++ List(v) }
                case P(v, l) => {
                  val _11 = {
                    val dummy1 = myvarlst ++ List(v)
                    check2(l)
                  }
                }
                case C(l1, l2) => {
                  val _8 = {
                    val dummy2 = check2(l1)
                    check2(l2)
                  }
                }
              }
            }
            val _12 = {
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
              val _13 = {
                val dummy = check2(lam)
                check3(myvarlst, myfreevarlst)
              }
            }
          }
        }
    }
  ) 
}