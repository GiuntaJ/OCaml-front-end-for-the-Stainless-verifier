import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub445 {
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
  
  def calc: Expr => Int63 = (
    (expr) =>
      {
        expr match {
          case NUM(i) => { i }
          case PLUS(lexpr, rexpr) => { calc(lexpr) + calc(rexpr) }
          case MINUS(lexpr, rexpr) => { calc(lexpr) - calc(rexpr) }
        }
    }
  )
  def eval: Formula => Boolean = (
    (formula) =>
      {
        formula match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(form) => {
            eval(form) match {
              case true => { false }
              case false => { true }
            }
          }
          case ANDALSO(lform, rform) => {
            eval(lform) match {
              case false => { false }
              case true => { eval(rform) }
            }
          }
          case ORELSE(lform, rform) => {
            eval(lform) match {
              case true => { true }
              case false => { eval(rform) }
            }
          }
          case IMPLY(lform, rform) => {
            eval(lform) match {
              case false => { true }
              case true => { eval(rform) }
            }
          }
          case LESS(lex, rex) => { if (calc(lex) < calc(rex)) true else false }
        }
    }
  )
}