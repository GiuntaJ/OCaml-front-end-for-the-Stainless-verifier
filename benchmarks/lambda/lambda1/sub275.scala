import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub275 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def check(((area, m))) = {
    m match {
      case STATION(_name) => { area.contains(_name) }
      case AREA(_name, _metro) => { check(_name :: area, _metro) }
      case CONNECT(_metro1, _metro2) => {
        check(area, _metro1) && check(area, _metro2)
      }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(_name) => { false }
      case AREA(_name, _metro) => { check(List(_name), _metro) }
      case CONNECT(_metro1, _metro2) => {
        checkMetro(_metro1) && checkMetro(_metro2)
      }
    }
  }
  
  
  
  
  
  
}
