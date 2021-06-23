import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub362 {
  /* SNU Programming Language Fall 2015
   * Homework 1 
   * Exercise 4: eval
   * Written by Dongho Kang 
   */
  
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
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        val _4 = {
          def ex_to_int: Expr => Int63 = (
            (e) =>
              {
                e match {
                  case NUM(e) => { e }
                  case PLUS(e1, e2) => { ex_to_int(e1) + ex_to_int(e2) }
                  case MINUS(e1, e2) => { ex_to_int(e1) - ex_to_int(e2) }
                }
            }
          )
          f match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(f1) => { if (eval(f1) == true) false else true }
            case ANDALSO(f1, f2) => {
              if (eval(f1) == true && eval(f2) == true) true else false
            }
            case ORELSE(f1, f2) => {
              if (eval(f1) == false && eval(f2) == false) false else true
            }
            case IMPLY(f1, f2) => {
              
                if (
                  eval(f1) == false
                ) {
                  true 
                } else if (
                  eval(f2) == true
                ) {
                  true 
                } else {
                  false
                }
            }
            case LESS(ex1, ex2) => { ex_to_int(ex1) < ex_to_int(ex2) }
          }
        }
    }
  )
}
