import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub108 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def isIn(((str, areas))) = {
    if (areas.filter(( (a) => { a == str } )) eq Nil()) false else true
  }
  
  def subChkMet(((met, areas))) = {
    met match {
      case STATION(n) => { if (isIn(n, areas)) true else false }
      case AREA(n, smet) => { subChkMet(smet, n :: areas) }
      case CONNECT(met1, met2) => {
        if (subChkMet(met1, areas) && subChkMet(met2, areas)) true else false
      }
    }
  }
  
  def checkMetro(met: Metro): Boolean = { subChkMet(met, Nil()) }
}