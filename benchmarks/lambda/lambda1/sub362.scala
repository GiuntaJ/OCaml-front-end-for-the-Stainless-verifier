import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub362 {
  /* 2015-11380 박찬양 HW2_4 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (mtr) =>
      {
        val _4 = {
          def checkIn(mtrr, arealist) = {
            mtrr match {
              case AREA(a, b) => { checkIn(b, a :: arealist) }
              case STATION(a) => { arealist.contains(a) }
              case CONNECT(a, b) => {
                checkIn(a, arealist) && checkIn(b, arealist)
              }
            }
          }
          checkIn(mtr, Nil())
        }
    }
  )
  
  	
}