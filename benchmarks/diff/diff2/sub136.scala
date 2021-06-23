import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub136 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
    
  def calcPower: (Aexp, String) => Aexp = {
    case (power, var0) =>
      {
        (power, var0) match {
          case (Power(variable, exponent), diffVar) => {
            
              if (
                variable == diffVar
              ) {
                Times(List(Const(exponent), Power(variable, exponent - 1))) 
              } else {
                Power(variable, exponent)
              }
          }
          case (_, _) => {
            assert(false, "Failure with wrong input for Power Calculation")
          }
        }
    }
  }
  	
  def contains: (List[Aexp], String) => Boolean = {
    case (l, var0) =>
      {
        (l, var0) match {
          case (Cons(hd, tl), diffVar) => {
            
              if (
                hd == Var(diffVar)
              ) {
                true 
              } else if (
                tl == Nil()
              ) {
                false 
              } else {
                contains(tl, diffVar)
              }
          }
          case (_, _) => {
            assert(false, "Failure with wrong input for contains check")
          }
        }
    }
  }
  		
  def noZero: List[Aexp] => Boolean = (
    (l) =>
      {
        l match {
          case Cons(hd, tl) => {
            
              if (
                hd == Const(0)
              ) {
                false 
              } else if (
                tl == Nil()
              ) {
                true 
              } else {
                noZero(tl)
              }
          }
          case _ => { assert(false, "Failure with wrong input for zero check") }
        }
    }
  )
  		
  def calcTimes: (List[Aexp], String) => List[Aexp] = {
    case (l, var0) =>
      {
        (l, var0) match {
          case (Nil(), diffVar) => { Nil() }
          case (Cons(Const(i), tl), diffVar) => {
            Const(i) :: calcTimes(tl, diffVar)
          }
          case (Cons(Var(s), tl), diffVar) => {
            
              if (
                s == diffVar
              ) {
                calcTimes(tl, diffVar) 
              } else {
                Var(s) :: calcTimes(tl, diffVar)
              }
          }
          case (Cons(Power(x, exponent), tl), diffVar) => {
            calcPower(Power(x, exponent), diffVar) :: calcTimes(tl, diffVar)
          }
          case (_, _) => {
            assert(false, "Failure with wrong input for multiplication")
          }
        }
    }
  }
  		
  	def calcSum: (List[Aexp], String) => List[Aexp] = {
    case (l, var0) =>
      {
        (l, var0) match {
          case (Nil(), diffVar) => { Nil() }
          case (Cons(Const(i), tl), diffVar) => {
            if (tl == Nil()) List(Const(0)) else calcSum(tl, diffVar)
          }
          case (Cons(Var(s), tl), diffVar) => {
            if (s == diffVar) List(Const(1)) else calcSum(tl, diffVar)
          }
          case (Cons(Power(variable, exponent), tl), diffVar) => {
            
              if (
                variable == diffVar
              ) {
                calcPower(Power(variable, exponent), diffVar) ::
                calcSum(tl, diffVar) 
              } else {
                calcSum(tl, diffVar)
              }
          }
          case (Cons(Times(x), tl), diffVar) => {
            
              if (
                contains(x, diffVar) && noZero(l)
              ) {
                calcTimes(x, diffVar) ++ calcSum(tl, diffVar) 
              } else {
                calcSum(tl, diffVar)
              }
          }
          case (_, _) => { assert(false, "Failure with wrong input for sum") }
        }
    }
  }
  		
  		
  val diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        (exp, var0) match {
          case (Const(c), diffVar) => { Const(0) }
          case (Var(s), diffVar) => { if (s == diffVar) Const(1) else Const(0) }
          case (Power(variable, exponent), diffVar) => {
            
              if (
                variable == diffVar
              ) {
                calcPower(Power(variable, exponent), diffVar) 
              } else {
                Const(0)
              }
          }
          case (Times(l), diffVar) => {
            
              if (
                contains(l, diffVar) && noZero(l)
              ) {
                Times(calcTimes(l, diffVar)) 
              } else {
                Const(0)
              }
          }
          case (Sum(l), diffVar) => { Sum(calcSum(l, diffVar)) }
        }
    }
  }		
}