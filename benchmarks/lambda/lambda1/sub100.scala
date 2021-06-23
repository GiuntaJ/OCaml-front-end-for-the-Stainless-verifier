import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub100 {
  /* HW1 exercise8 2009-11697 Kim HyunJoon */
  /* CheckMetroMap */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (met) =>
      {
        val _4 = {
          def myCheckMetro(m, lst) = {
            m match {
              case STATION(s) => { lst.contains(s) }
              case AREA(a, n) => { myCheckMetro(n, a :: lst) }
              case CONNECT(m1, m2) => {
                myCheckMetro(m1, lst) && myCheckMetro(m2, lst)
              }
            }
          }
          myCheckMetro(met, Nil())
        }
    }
  )
}
