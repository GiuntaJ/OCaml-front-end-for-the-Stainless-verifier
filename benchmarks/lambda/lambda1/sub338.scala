import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub338 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def validMetro(((m, names))) = {
    m match {
      case STATION(name) => { names.exists(( (x) => { x == name } )) }
      case AREA(name, m0) => { validMetro(m0, name :: names) }
      case CONNECT(m1, m2) => { validMetro(m1, names) && validMetro(m2, names) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { validMetro(m, Nil()) }
}