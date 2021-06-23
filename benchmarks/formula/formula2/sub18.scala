import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub18 {
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
  
  /*let rec exp : int -> int
  = fun n ->
  	match n with
  	| Num k -> k
  	| Plus (k, l) -> exp(k) + exp(l)
  	| Minus (k, l) -> exp(k) - exp(l);;*/
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(p) => { not(eval(p)) }
          case AndAlso(p, q) => { eval(p) && eval(q) }
          case OrElse(p, q) => { eval(p) || eval(q) }
          case Imply(p, q) => { not(eval(p)) || eval(q) }
          case Equal(p, q) => { p == q }
        }
    }
  )
  		/*if exp(p) = exp(q) then true
  		else false;;*/
  
  
  
}
