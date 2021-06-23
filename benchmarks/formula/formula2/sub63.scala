import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub63 {
  sealed abstract class Formula {}
  case object True_ extends Formula {}
  case object False_ extends Formula {}
  case class Not(param0: Formula) extends Formula {}
  case class AndAlso(param0: Formula,  param1: Formula) extends Formula {}
  case class OrElse(param0: Formula,  param1: Formula) extends Formula {}
  case class Imply(param0: Formula,  param1: Formula) extends Formula {}
  case class Equal(param0: Exp,  param1: Exp) extends Formula {}
  
  sealed abstract class Exp {}
  case class Num(param0: Int63) extends Exp {}
  case class Plus(param0: Exp,  param1: Exp) extends Exp {}
  case class Minus(param0: Exp,  param1: Exp) extends Exp {} 
  
  def eval_exp(e: Exp): Int63 = {
    e match {
      case Num(i) => { i }
      case Plus(e1, e2) => { eval_exp(e1) + eval_exp(e2) }
      case Minus(e1, e2) => { eval_exp(e1) - eval_exp(e2) }
    }
  } 
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f1) => {
            val _16 = {
              val v1 = eval(f1)
              v1 match {
                case true => { false }
                case false => { true }
              }
            }
          }
          case AndAlso(f1, f2) => {
            val _12 = {
              val v1 = eval(f1)
              val _13 = {
                val v2 = eval(f2)
                (v1, v2) match {
                  case (true, true) => { true }
                  case (_, _) => { false }
                }
              }
            }
          }
          case OrElse(f1, f2) => {
            val _6 = {
              val v1 = eval(f1)
              v1 match {
                case true => { true }
                case _ => {
                  val _9 = {
                    val v2 = eval(f2)
                    v2 match {
                      case true => { true }
                      case _ => { false }
                    }
                  }
                }
              }
            }
          }
          case Imply(f1, f2) => {
            val _2 = {
              val v1 = eval(f1)
              val _3 = {
                val v2 = eval(f2)
                (v1, v2) match {
                  case (true, _) => { v2 }
                  case (_, _) => { true }
                }
              }
            }
          }
          case Equal(e1, e2) => { eval_exp(e1) == eval_exp(e2) }
        }
    }
  )
}