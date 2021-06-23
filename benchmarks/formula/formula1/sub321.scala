import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub321 {
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
  
  def makeint(ex: Expr): Int63 = {
    ex match {
      case NUM(i) => { i }
      case PLUS(lex1, rex1) => { makeint(lex1) + makeint(rex1) }
      case MINUS(lex2, rex2) => { makeint(lex2) - makeint(rex2) }
    }
  }
  
  
  
  def eval(form1: Formula): Boolean = {
    form1 match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(form1_0) => { not(eval(form1_0)) }
      case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
      case ORELSE(f3, f4) => { eval(f3) || eval(f4) }
      case IMPLY(f5, f6) => { not(eval(f5)) || eval(f6) }
      case LESS(ex1, ex2) => { if (makeint(ex1) < makeint(ex2)) true else false
      }
    }
  }
}