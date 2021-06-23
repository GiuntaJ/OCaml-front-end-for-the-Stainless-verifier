import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub288 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(mt: Metro): Boolean = { ckM(mt, Nil()) }
  def ckM(mt, l) = {
    mt match {
      case STATION(nm) => { if_cont(nm, l) }
      case AREA(nm, mt_a) => { ckM(mt_a, nm :: l) }
      case CONNECT(mt_a, mt_b) => { ckM(mt_a, l) && ckM(mt_b, l) }
    }
  }
  def if_cont(nm, l) = { l.exists(equal(nm)) }
  def equal(nm_a, nm_b) = { if (nm_a == nm_b) true else false }
}