import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub400 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (m) =>
      {
        m match {
          case STATION(_) => { false }
          case CONNECT(_) => { false }
          case AREA(n_0, m_0) => { listMetro(m_0).contains(n_0) }
        }
    }
  )
  def listMetro: Metro => List[A] = (
    (m) =>
      {
        m match {
          case STATION(m_0) => { List(m_0) }
          case CONNECT(m1, m2) => {
            (listMetro(m1), listMetro(m2)) match {
              case (Cons(a, b), Cons(c, d)) => { a :: b ++(c :: d) }
              case (_, _) => { Nil() }
            }
          }
          case AREA(n_0, m_0) => {
            checkMetro(m) match {
              case true => { listMetro(m_0) }
              case false => { Nil() }
            }
          }
        }
    }
  )
  
  
  /* TESTING FIELD BELOW */
  
}
