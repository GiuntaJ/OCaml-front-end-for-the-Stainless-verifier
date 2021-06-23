import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub36 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  val checkMetro: Metro => Boolean = val _0 = {
    def cM(((met, lst))) = {
      met match {
        case STATION(n) => { lst.contains(n) }
        case AREA(n, m) => { cM(m, n :: lst) }
        case CONNECT(m1, m2) => { cM(m1, lst) && cM(m2, lst) }
      }
    }
    ( (m) => { cM(m, Nil()) } )
  }
}