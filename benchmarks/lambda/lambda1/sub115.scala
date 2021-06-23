import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub115 {
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  sealed abstract class Stack {}
  case object NONE extends Stack {}
  case class STACK(param0: Name,  param1: Stack) extends Stack {}
  
  
  def stationinStack(((name, stk))) = {
    stk match {
      case NONE => { false }
      case STACK(nm, st) => { if (nm == name) true else stationinStack(name, st)
      }
    }
  }
  
  def myCheckMetro(((met, stk))) = {
    met match {
      case STATION(nm) => { stationinStack(nm, stk) }
      case AREA(nm, me) => { myCheckMetro(me, STACK(nm, stk)) }
      case CONNECT(mea, meb) => {
        myCheckMetro(mea, stk) && myCheckMetro(meb, stk)
      }
    }
  }
  
  def checkMetro(met: Metro): Boolean = { myCheckMetro(met, NONE) }
}