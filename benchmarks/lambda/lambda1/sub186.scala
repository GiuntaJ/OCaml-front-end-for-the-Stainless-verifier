import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub186 {
  /*
  	department : computer science & engineering
  	student ID : 2012-11242 / name : Seon-bi, Park
  */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checking(mtr: Metro, arealst: List[Name]): Boolean = {
    mtr match {
      case STATION(n) => { if (arealst.contains(n) == true) true else false }
      case CONNECT(m, n) => { checking(m, arealst) && checking(n, arealst) }
      case AREA(n, m) => { checking(m, n :: arealst) }
    }
  }
  
  def checkMetro(mtr: Metro): Boolean = {
    mtr match {
      case STATION(n) => { false }
      case CONNECT(m, n) => { checking(m, Nil()) && checking(n, Nil()) }
      case AREA(n, m) => { checking(mtr, Nil()) }
    }
  }
}