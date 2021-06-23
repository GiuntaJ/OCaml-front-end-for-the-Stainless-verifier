import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub205 {
  /* file name : ex4.ml */
  /* author : Jisoon Park (jspark@ropas.snu.ac.kr) */
  /* date : 2013-09-13 */
  /* Exercise 4 */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroSub: (Metro, List[String]) => Boolean = {
    case (mtr, areaList) =>
      {
        mtr match {
          case STATION(st) => { areaList.exists(( (x) => { x == st } )) }
          case AREA(ar, mtr2) => { checkMetroSub(mtr2, ar :: areaList) }
          case CONNECT(mtr1, mtr2) => {
            checkMetroSub(mtr1, areaList) && checkMetroSub(mtr2, areaList)
          }
        }
    }
  } 
  
  val checkMetro: Metro => Boolean = ( (mtr) => { checkMetroSub(mtr, Nil()) } )
}