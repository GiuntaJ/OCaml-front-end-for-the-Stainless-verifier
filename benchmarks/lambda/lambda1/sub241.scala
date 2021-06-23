import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub241 {
  
  
  /* Author: Arif Jafer, 2012-11255 */
  /* PL, Spring 2014 */
  
  /* HW2-Q1: checkMetroMap */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def is_member[A](y: A, lst: List[A]): Boolean = {
    lst match {
      case Nil() => { false }
      case Cons(x, xl) => { x == y || is_member(y, xl) }
    }
  }
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def checkMetroRec(metro, accm) = {
            metro match {
              case STATION(id) => { is_member(id, accm) }
              case AREA(id, m1) => { checkMetroRec(m1, id :: accm) }
              case CONNECT(m1, m2) => {
                checkMetroRec(m1, accm) && checkMetroRec(m2, accm)
              }
            }
          }
          checkMetroRec(m, Nil())
        }
    }
  )
  
  
}
