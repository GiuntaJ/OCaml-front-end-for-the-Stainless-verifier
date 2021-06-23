import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub309 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def check: (List[String], Metro) => Boolean = {
    case (nlist, m) =>
      {
        
          if (
            nlist == Nil()
          ) {
            true 
          } else {
            m match {
              case STATION(n) => { nlist.exists(( (x) => { n == x } )) }
              case AREA(n, m1) => { check(n :: nlist, m1) }
              case CONNECT(m1, m2) => { check(nlist, m1) && check(nlist, m2) }
            }
          }
    }
  }
  
  def checkMetro: Metro => Boolean = (
    (m) =>
      {
        m match {
          case STATION(n) => { false }
          case AREA(n, m1) => { check(List(n), m1) }
          case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
        }
    }
  )
}