import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub149 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def deletefrom: (List[Var], Var) => List[Var] = {
    case (varlist1, vardel) =>
      {
        varlist1 match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            
              if (
                hd == vardel
              ) {
                deletefrom(tl, vardel) 
              } else {
                List(hd) ++ deletefrom(tl, vardel)
              }
          }
        }
    }
  }
  def addfor: (List[Var], Var) => List[Var] = {
    case (varlist1, varadd) =>
      {
        varlist1 match {
          case Nil() => { List(varadd) }
          case Cons(hd, tl) => {
            if (hd == varadd) varlist1 else List(hd) ++ addfor(tl, varadd)
          }
        }
    }
  }
  def check2: (Exp, List[Var]) => List[Var] = {
    case (exp, varlist1) =>
      {
        exp match {
          case V(var1) => { addfor(varlist1, var1) }
          case P(var1, exp1) => { deletefrom(check2(exp1, varlist1), var1) }
          case C(exp1, exp2) => {
            check2(exp1, varlist1) ++ check2(exp2, varlist1)
          }
        }
    }
  }
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        check2(exp, Nil()) match {
          case Nil() => { true }
          case _ => { false }
        }
    }
  )
}
