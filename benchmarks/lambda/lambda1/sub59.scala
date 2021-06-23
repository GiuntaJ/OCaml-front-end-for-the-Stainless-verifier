import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub59 {
  /* PL HW1-7 "Check Metro Map"
     2007-11738
     알렉산더 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /* checkMetro: metro -> bool */
  def checkMetro(mtr: Metro): Boolean = {
    val _2 = {
      def checkMetroInner(((met, aNameList))) = {
        met match {
          case STATION(stName) => {
            if (aNameList.contains(stName)) true else false
          }
          case CONNECT(a, b) => {
            checkMetroInner(a, aNameList) && checkMetroInner(b, aNameList)
          }
          case AREA(aName, CONNECT(a, b)) => {
            checkMetroInner(CONNECT(AREA(aName, a), AREA(aName, b)), aNameList)
          }
          case AREA(aName, m) => { checkMetroInner(m, aName :: aNameList) }
        }
      }
      checkMetroInner(mtr, Nil())
    }
  }
  
  /*
      aName - AREA name
      stName - STATION name
      aNameList - list of AREA names
  */
}