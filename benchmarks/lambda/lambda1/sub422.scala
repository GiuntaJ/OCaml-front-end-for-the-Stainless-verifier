import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub422 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkArea(((mc: Metro, l: List[String]))): Boolean = {
        mc match {
          case STATION(nd) => { l.contains(nd) }
          case CONNECT(md1, md2) => { checkArea(md1, l) && checkArea(md2, l) }
          case AREA(nd, md) => { checkArea(md, l ++(List(nd))) }
        }
      }
      m match {
        case STATION(n) => { false }
        case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
        case AREA(ns, ms) => { checkArea(m, Nil()) }
      }
    }
  }
}