import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub308 {
  /* 2011-10915 / 생명과학부/ 신지민/ Homework 1-4 */
  
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
  
  
  def cal: Expr => Int63 = (
    (e) =>
      {
        e match {
          case NUM(i) => { i }
          case PLUS(e1, e2) => {
            val _6 = {
              val i1 = cal(e1)
              val _7 = {
                val i2 = cal(e2)
                i1 + i2
              }
            }
          }
          case MINUS(e1, e2) => {
            val _2 = {
              val i1 = cal(e1)
              val _3 = {
                val i2 = cal(e2)
                i1 - i2
              }
            }
          }
        }
    }
  )
  
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(a) => { if (eval(a) eq true) false else true }
          case ANDALSO(a, b) => {
            if (eval(a) eq true && eval(b) eq true) true else false
          }
          case ORELSE(a, b) => {
            if (eval(a) eq true || eval(b) eq true) true else false
          }
          case IMPLY(a, b) => {
            if (eval(a) eq true && eval(b) eq false) false else true
          }
          case LESS(a, b) => { if (cal(a) < cal(b)) true else false }
        }
    }
  )
  
  /*
  let b = eval TRUE
  let _= begin
  	if(b==true) then  print_endline("true
  ") 
  	else print_endline("false
  ")
  	end
  
  let b = eval (NOT(ANDALSO(TRUE,TRUE)))
  let _= begin
  	if(b==true) then  print_endline("true
  ") 
  	else print_endline("false
  ")
  	end
  
  let b = eval (IMPLY((LESS(PLUS(NUM 1,NUM 4), MINUS(NUM 1000, NUM 10)),ANDALSO(FALSE,FALSE))))
  let _= begin
  	if(b==true) then  print_endline("true
  ") 
  	else print_endline("false
  ")
  	end
  */
}