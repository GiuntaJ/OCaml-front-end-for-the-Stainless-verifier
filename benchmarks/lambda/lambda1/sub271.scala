import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub271 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def comparing(((name, li))) = {
    li match {
      case Nil() => { false }
      case Cons(st, li_0) => { if (name == st) true else comparing(name, li_0) }
    }
  }
  		
  	def buff(((under, li))) = {
    under match {
      case STATION(st_name) => { comparing(st_name, li) }
      case AREA(id, m) => { buff(m, li ++(List(id))) }
      case CONNECT(m1, m2) => { buff(m1, li) && buff(m2, li) }
    }
  }
  		
  
  def checkMetro(under: Metro): Boolean = { buff(under, Nil()) }
  		
}