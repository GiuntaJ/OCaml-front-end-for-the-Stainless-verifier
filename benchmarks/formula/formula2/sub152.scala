import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub152 {
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
          def eval_0(e) = {
            e match {
              case Num(_) => { 0 }
              case Plus(e1, e2) => { eval_0(e1) + eval_0(e2) }
              case Minus(e1, e2) => { eval_0(e1) - eval_0(e2) }
            }
          }
          f match {
            case Equal(e1, e2) => {
              if (eval_0(e1) == eval_0(e2)) true else false
            }
          }
        }
    }
  )
}
