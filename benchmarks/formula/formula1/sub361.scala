import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub361 {
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
  
  def eval2: Expr => Int63 = (
    (a) =>
      {
        a match {
          case NUM(a) => { a }
          case PLUS(expr_1, expr_2) => {
            val _6 = {
              val num1 = eval2(expr_1)
              val _7 = {
                val num2 = eval2(expr_2)
                num1 + num2
              }
            }
          }
          case MINUS(expr_1, expr_2) => {
            val _2 = {
              val num1 = eval2(expr_1)
              val _3 = {
                val num2 = eval2(expr_2)
                num1 - num2
              }
            }
          }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (a) =>
      {
        a match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(formula_1) => {
            val _26 = {
              val b = eval(formula_1)
              if (b == true) false else true
            }
          }
          case ANDALSO(formula_1, formula_2) => {
            val _22 = {
              val bool1 = eval(formula_1)
              val _23 = {
                val bool2 = eval(formula_2)
                bool1 && bool2
              }
            }
          }
          case ORELSE(formula_1, formula_2) => {
            val _18 = {
              val bool1 = eval(formula_1)
              val _19 = {
                val bool2 = eval(formula_2)
                bool1 || bool2
              }
            }
          }
          case IMPLY(formula_1, formula_2) => {
            val _14 = {
              val bool1 = eval(formula_1)
              val _15 = {
                val bool2 = eval(formula_2)
                if (bool1 == true && bool2 == false) false else true
              }
            }
          }
          case LESS(expr_1, expr_2) => {
            val _10 = {
              val num1 = eval2(expr_1)
              val _11 = {
                val num2 = eval2(expr_2)
                if (num1 < num2) true else false
              }
            }
          }
        }
    }
  )
  
}
