import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub158 {
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
  
  def expr: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(num) => { num }
          case Plus(left, right) => { expr(left) + expr(right) }
          case Minus(left, right) => { expr(left) - expr(right) }
        }
    }
  )
  
  val eval: Formula => Boolean = (
    (f) =>
      {
        val _4 = {
          def div(f) = {
            f match {
              case True_ => { true }
              case False_ => { false }
              case Not(ev) => { if (div(ev)) false else true }
              case AndAlso(left, right) => { div(left) && div(right) }
              case OrElse(left, right) => { div(left) || div(right) }
              case Imply(left, right) => { div(Not(left)) || div(right) }
              case Equal(left, right) => { expr(left) == expr(right) }
            }
          }
          div(f)
        }
    }
  )
}