import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub146 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def isthere: (String, Exp) => Exp = {
    case (key, exp) =>
      {
        exp match {
          case V(v1) => { if (key == v1) V("true") else V(v1) }
          case P(v1, e1) => { P(v1, isthere(key, isthere(v1, e1))) }
          case C(e1, e2) => { C(isthere(key, e1), isthere(key, e2)) }
        }
    }
  }
  
  	def nothere: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(v) => { if (v == "true") true else false }
          case P(v, e) => { if (nothere(e) == true) true else false }
          case C(e1, e2) => {
            if (nothere(e1) == true && nothere(e2) == true) true else false
          }
        }
    }
  )
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(v) => { false }
          case P(v, e) => { nothere(isthere(v, e)) }
          case C(e1, e2) => { false }
        }
    }
  )
}