import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub219 {
  /********************************
   ** PL::HW[02].Problem[01]      **
   **                             **
   ** Mod. Init: 2014-09-25 19:16 **
   ** Mod. Fin.: 2014-09-25 21:24 **
   **                             **
   ** Writ. by : CMS              **
   *********************************/
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroMap(area_list: List[Name], target: Metro): Boolean = {
    target match {
      case STATION(name) => { if (area_list.contains(name)) true else false }
      case AREA(id, m) => {
        
          if (
            isName(id)
          ) {
            
              if (
                area_list.contains(id)
              ) {
                checkMetroMap(area_list, m) 
              } else {
                checkMetroMap(id :: area_list, m)
              } 
          } else {
            false
          }
      }
      case CONNECT(m1, m2) => {
        
          if (
            checkMetroMap(area_list, m1)
          ) {
            checkMetroMap(area_list, m2) 
          } else {
            false
          }
      }
    }
  }
  def isName(str) = {
    str match {
      case str1 => { true }
    }
  }
  
  val checkMetro: Metro => Boolean = checkMetroMap(Nil())
}
