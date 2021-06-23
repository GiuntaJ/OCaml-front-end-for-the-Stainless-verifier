import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub369 {
  /* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-1 */
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
  
  def compute: Expr => Int63 = (
    (e) =>
      {
        e match {
          case NUM(i) => { i }
          case PLUS(f, g) => { compute(f) + compute(g) }
          case MINUS(f, g) => { compute(f) - compute(g) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (a) =>
      {
        a match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(b) => {
            eval(b) match {
              case true => { false }
              case false => { true }
            }
          }
          case ANDALSO(b, c) => {
            (eval(b), eval(c)) match {
              case (true, true) => { true }
              case _ => { false }
            }
          }
          case ORELSE(b, c) => {
            (eval(b), eval(c)) match {
              case (false, false) => { false }
              case _ => { true }
            }
          }
          case IMPLY(b, c) => {
            (eval(b), eval(c)) match {
              case (true, false) => { false }
              case _ => { true }
            }
          }
          case LESS(e, f) => { compute(e) < compute(f) }
        }
    }
  )
  /* Test Code
  let e : expr = PLUS (NUM 3, MINUS (NUM 8, NUM 4))
  let f : expr = PLUS (NUM 2, NUM 6)
  let g : expr = NUM 5
  let x : formula = NOT (ORELSE (IMPLY(LESS(e,f), LESS(g, f)), ANDALSO(FALSE, TRUE)))
  let y : formula = LESS(g, e)
  let z : formula = IMPLY(x, y)
  let w : formula = IMPLY(y, x)
  let t : formula = ANDALSO(z, w)
  let s : formula = ORELSE(w, z)
  
  let test b = match eval (b) with
      | false -> print_endline ("false")
      | true -> print_endline ("true")
  
  let _ = test (x)
  let _ = test (y)
  let _ = test (z)
  let _ = test (w)
  let _ = test (t)
  let _ = test (s)
  */
}