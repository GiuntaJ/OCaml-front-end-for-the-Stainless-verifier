import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub401 {
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
  
  
  def plmi: Expr => Int63 = (
    (exp) =>
      {
        exp match {
          case NUM(n) => { n }
          case PLUS(n1, n2) => { plmi(n1) + plmi(n2) }
          case MINUS(n1, n2) => { plmi(n1) - plmi(n2) }
        }
    }
  )
  
  val eval: Formula => Boolean = (
    (fm) =>
      {
        fm match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(fm1) => { if (fm1 == TRUE) false else true }
          case ANDALSO(fm1, fm2) => {
            
              if (
                fm1 == FALSE
              ) {
                false 
              } else if (
                fm2 == FALSE
              ) {
                false 
              } else {
                true
              }
          }
          case ORELSE(fm1, fm2) => {
            
              if (
                fm1 == TRUE
              ) {
                true 
              } else if (
                fm2 == TRUE
              ) {
                true 
              } else {
                false
              }
          }
          case IMPLY(fm1, fm2) => {
            if (fm1 == TRUE && fm2 == FALSE) false else true
          }
          case LESS(exp1, exp2) => {
            if (plmi(exp1) < plmi(exp2)) true else false
          }
        }
    }
  )
}