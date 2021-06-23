import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub69 {
  sealed abstract class Exp {}
  case class V(param0: Name) extends Exp {}
  case class P(param0: Name,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Name = String 
  
  def checkWellFormed(expression: Exp, checkList: List[Name]): Boolean = {
    expression match {
      case V(u) => { checkList.contains(u) }
      case P(u, v) => { checkWellFormed(v, u :: checkList) }
      case C(u, v) => {
        checkWellFormed(u, checkList) && checkWellFormed(v, checkList)
      }
    }
  }
  
  val check: Exp => Boolean = ( (expression) => { checkWellFormed(expression, Nil()) } )
}