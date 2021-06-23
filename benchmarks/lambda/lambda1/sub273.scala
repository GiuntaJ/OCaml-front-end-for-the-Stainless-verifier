import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub273 {
  /*
   * Brief      : HW2, Program Language (4190.310)
   * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
   * Student Id : 2014-21767
   * Date       : Sep. 23, 2014
   */
  
  /* Exercise 1 : CheckMetroMap */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def checkMetroSub: (List[Name], Metro) => Boolean = {
            case (nl, m) =>
              {
                m match {
                  case STATION(name) => { nl.contains(name) }
                  case AREA(name, metro) => { checkMetroSub(name :: nl, metro) }
                  case CONNECT(metro1, metro2) => {
                    checkMetroSub(nl, metro1) && checkMetroSub(nl, metro2)
                  }
                }
            }
          }
          checkMetroSub(Nil(), m)
        }
    }
  )
}