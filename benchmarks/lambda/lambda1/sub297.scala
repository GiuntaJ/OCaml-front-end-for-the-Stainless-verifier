import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub297 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkSt_in_Metro(((name, metro))) = {
    metro match {
      case STATION(nm) => { if (nm == name) true else false }
      case AREA(nm, met) => { checkSt_in_Metro(name, met) }
      case CONNECT(met1, met2) => {
        checkSt_in_Metro(name, met1) || checkSt_in_Metro(name, met2)
      }
    }
  }
  
  def makeMetro_wo_St(((name, metro))) = {
    metro match {
      case STATION(nm) => {
        if (nm == name) STATION("2011-13343ysh") else STATION(nm)
      }
      case AREA(nm, met) => { AREA(nm, makeMetro_wo_St(name, met)) }
      case CONNECT(met1, met2) => {
        CONNECT(makeMetro_wo_St(name, met1), makeMetro_wo_St(name, met2))
      }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(name) => { if (name == "2011-13343ysh") true else false }
      case AREA(name, metro) => {
        
          if (
            checkSt_in_Metro(name, metro)
          ) {
            checkMetro(makeMetro_wo_St(name, metro)) 
          } else {
            checkMetro(metro)
          }
      }
      case CONNECT(metro1, metro2) => { checkMetro(metro1) && checkMetro(metro2)
      }
    }
  }
}