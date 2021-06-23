import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub165 {
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
  
  val eval: Formula => Boolean = (
    (f) =>
      {
        val _4 = {
          def evaluate(sik) = {
            sik match {
              case True_ => { true }
              case False_ => { false }
              case Not(a) => { false == evaluate(a) }
              case AndAlso(a, b) => { evaluate(a) && evaluate(b) }
              case OrElse(a, b) => { evaluate(a) || evaluate(b) }
              case Imply(a, b) => { false == evaluate(a) || evaluate(b) }
              case Equal(n, m) => {
                val _7 = {
                  def expr(ex) = {
                    ex match {
                      case Num(n) => { n }
                      case Plus(n, m) => { expr(n) + expr(m) }
                      case Minus(n, m) => { expr(n) - expr(m) }
                    }
                  }
                  expr(n) == expr(m)
                }
              }
            }
          }
          evaluate(f)
        }
    }
  )
  
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
}