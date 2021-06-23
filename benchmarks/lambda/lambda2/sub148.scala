import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub148 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
  def getBoundValues: Exp => List[Var] = (
    (exp) =>
      {
        exp match {
          case V(v) => { Nil() }
          case P(v, e) => { v :: getBoundValues(e) }
          case C(e1, e2) => { getBoundValues(e1) ++ getBoundValues(e2) }
        }
    }
  )
  	
  def getValues: Exp => List[Var] = (
    (exp) =>
      {
        exp match {
          case V(v) => { List(v) }
          case P(v, e) => { getValues(e) }
          case C(e1, e2) => { getValues(e1) ++ getValues(e2) }
        }
    }
  )
  	
  def compareHelper: (Var, List[Var]) => Boolean = {
    case (var0, varL) =>
      {
        (var0, varL) match {
          case (v, Cons(hd, tl)) => {
            
              if (
                v == hd
              ) {
                true 
              } else if (
                tl == Nil()
              ) {
                false 
              } else {
                compareHelper(v, tl)
              }
          }
        }
    }
  }
  	
  def compare: (List[Var], List[Var]) => Boolean = {
    case (values, bound) =>
      {
        (values, bound) match {
          case (Cons(valueHd, valueTl), bound) => {
            
              if (
                valueTl == Nil()
              ) {
                compareHelper(valueHd, bound) 
              } else if (
                compareHelper(valueHd, bound)
              ) {
                compare(valueTl, bound) 
              } else {
                false
              }
          }
        }
    }
  }
  	
    val check: Exp => Boolean = ( (exp) => { compare(getValues(exp), getBoundValues(exp)) } )
}