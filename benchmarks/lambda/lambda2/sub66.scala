import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub66 {
  	sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def elist: Exp => List[Var] = (
    (exp) =>
      {
        exp match {
          case V(var0) => { Nil() }
          case P(var0, exp1) => {
            exp1 match {
              case V(var2) => { List(var0) }
              case P(var2, exp2) => { List(var0, var2) ++ elist(exp2) }
              case C(exp3, exp4) => { List(var0) ++ elist(exp3) ++ elist(exp4) }
            }
          }
          case C(exp2, exp3) => {
            exp2 match {
              case V(var0) => { elist(exp3) }
              case P(var2, exp4) => { List(var2) ++ elist(exp4) }
              case C(exp5, exp6) => { elist(exp5) ++ elist(exp6) ++ elist(exp3)
              }
            }
          }
        }
    }
  )
  
  	def vlist(exp) = {
    exp match {
      case V(var0) => { List(var0) }
      case P(var0, exp1) => {
        exp1 match {
          case V(var2) => { List(var2) }
          case P(var2, exp2) => { vlist(exp2) }
          case C(exp3, exp4) => { vlist(exp3) ++ vlist(exp4) }
        }
      }
      case C(exp2, exp3) => {
        exp2 match {
          case V(var0) => { List(var0) ++ vlist(exp3) }
          case P(var2, exp4) => { vlist(exp4) }
          case C(exp5, exp6) => { vlist(exp5) ++ vlist(exp6) ++ vlist(exp3) }
        }
      }
    }
  }
  		  
  		  
  	def exist(explist, varlist) = {
    varlist match {
      case Nil() => { true }
      case Cons(vhd, vtl) => {
        explist match {
          case Nil() => { false }
          case Cons(ehd, etl) => {
            
              if (
                vhd == ehd
              ) {
                exist(explist, vtl) 
              } else {
                exist(etl, List(vhd)) || exist(etl, varlist)
              }
          }
        }
      }
    }
  }
  		  
  	def check: Exp => Boolean = ( (exp) => { exist(elist(exp), vlist(exp)) } )
}