import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub184 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def check(((m, l))) = {
    m match {
      case AREA(nam, met) => { check(met, nam :: l) }
      case STATION(nam) => {
        val _2 = {
          val x = List.find(( (elem) => { elem == nam } ), l)
          true
        }
      }
      case CONNECT(met1, met2) => { check(met1, l) && check(met2, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { check(m, Nil()) }
}