import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub361 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def solver(((ln, mt))): Boolean = {
    mt match {
      case STATION(n) => { ln.contains(n) }
      case AREA(n, m) => { solver(ln ++ List(n), m) }
      case CONNECT(m1, m2) => { solver(ln, m1) && solver(ln, m2) }
    }
  }
  def checkMetro(mt: Metro): Boolean = { solver(Nil(), mt) }
}