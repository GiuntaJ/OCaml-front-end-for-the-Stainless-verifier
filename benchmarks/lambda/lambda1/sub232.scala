import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub232 {
  sealed case class TODO() extends Exception {}
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def checkMetro(m: Metro): Boolean = {
    m match {
      case AREA(id, y) => {
        
          if (
            checkMetro(y)
          ) {
            true 
          } else {
            y match {
              case STATION(s) => { s == id }
              case AREA(a, metr) => {
                metr match {
                  case CONNECT(m1, m2) => {
                    (checkMetro(AREA(a, m1)) || checkMetro(AREA(id, m1))) &&
                    (checkMetro(AREA(a, m2)) || checkMetro(AREA(id, m2)))
                  }
                  case _ => {
                    checkMetro(AREA(id, metr)) || checkMetro(AREA(a, metr))
                  }
                }
              }
              case CONNECT(m1, m2) => {
                checkMetro(AREA(id, m1)) && checkMetro(AREA(id, m2))
              }
            }
          }
      }
      case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
      case _ => { false }
    }
  }
}