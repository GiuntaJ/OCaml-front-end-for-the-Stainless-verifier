import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub196 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def deletecity(((name, lis))) = {
    
      if (
        name == lis.head
      ) {
        if (lis.length == 1) Nil() else deletecity(name, lis.tail) 
      } else if (
        lis.length == 1
      ) {
        lis 
      } else {
        lis.head :: deletecity(name, lis.tail)
      }
  }
  def findcity(met: Metro): List[Name] = {
    met match {
      case STATION(name) => { List(name) }
      case AREA(name, metro) => { deletecity(name, findcity(metro)) }
      case CONNECT(met1, met2) => { findcity(met1) reverse_:::(findcity(met2)) }
    }
  }
  
  def deletestation(((name, metro))) = {
    val _2 = {
      val citylist = findcity(metro)
      if (deletecity(name, citylist) == Nil()) true else false
    }
  }
  
  def checkMetro(met: Metro): Boolean = {
    met match {
      case STATION(name) => { false }
      case AREA(name, metro) => {
        if (checkMetro(metro)) true else deletestation(name, metro)
      }
      case CONNECT(met1, met2) => { checkMetro(met1) && checkMetro(met2) }
    }
  }
}