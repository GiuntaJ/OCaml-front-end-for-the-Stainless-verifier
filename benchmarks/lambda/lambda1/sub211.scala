import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub211 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro_aux: (Metro, List[Name]) => Boolean = {
    case (m, nl) =>
      {
        m match {
          case STATION(n) => { nl.exists(( (n1) => { n == n1 } )) }
          case AREA(n, m1) => { checkMetro_aux(m1, n :: nl) }
          case CONNECT(m1, m2) => {
            checkMetro_aux(m1, nl) && checkMetro_aux(m2, nl)
          }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = ( (m) => { checkMetro_aux(m, Nil()) } )
}