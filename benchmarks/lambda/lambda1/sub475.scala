import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub475 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
    def checkMetro: Metro => Boolean = (
    (x) =>
      {
        x match {
          case AREA(n, STATION(n1)) => { if (n1 == n) true else false }
          case AREA(n, AREA(n1, m)) => {
            checkMetro(AREA(n, m)) && checkMetro(AREA(n1, m))
          }
          case AREA(n, CONNECT(STATION(m1), STATION(m2))) => {
            if (n == m1 || n == m2) true else false
          }
          case AREA(n, CONNECT(AREA(n1, m1), STATION(n2))) => {
            
              if (
                (n == n2 || n1 == n2) &&
                (checkMetro(AREA(n, m1)) || checkMetro(AREA(n1, m1)))
              ) {
                true 
              } else {
                false
              }
          }
          case AREA(n, CONNECT(STATION(n2), AREA(n1, m1))) => {
            
              if (
                (n == n2 || n1 == n2) &&
                (checkMetro(AREA(n, m1)) || checkMetro(AREA(n1, m1)))
              ) {
                true 
              } else {
                false
              }
          }
          case CONNECT(AREA(n1, m1), AREA(n2, m2)) => {
            
              if (
                (checkMetro(AREA(n1, m1)) || checkMetro(AREA(n2, m1))) &&
                (checkMetro(AREA(n1, m2)) || checkMetro(AREA(n2, m2)))
              ) {
                true 
              } else {
                false
              }
          }
          case STATION(n) => { false }
        }
    }
  )
}