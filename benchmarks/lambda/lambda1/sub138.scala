import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub138 {
  /* ID : 2007-12138 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /* The main idea is simple; 
      if we succeed extracting all Area name and Station name in lists,
      we can compare it and findout it is correct or not.
  */
  
  
  /* My functions
     list_station  : returns a list of station name of target(metro type)
     list_area     : returns a list of area name of target(metro type)
     list_matching : gets two lists and check if every element in list1 is in list2. If so, return true.
  */
  def list_station(ipt: Metro): List[Name] = {
    ipt match {
      case STATION(a) => { List(a) }
      case AREA(a, m) => { list_station(m) }
      case CONNECT(m1, m2) => { list_station(m1) ++ list_station(m2) }
    }
  }
  
  def list_area(ipt: Metro): List[Name] = {
    ipt match {
      case STATION(a) => { Nil() }
      case AREA(a, m) => { a :: list_area(m) }
      case CONNECT(m1, m2) => { list_area(m1) ++ list_area(m2) }
    }
  }
  
  def list_matching(((ipt1, ipt2))) = {
    ipt1 match {
      case Nil() => { true }
      case _ => {
        if (ipt2.contains(ipt1.head)) list_matching(ipt1.tail, ipt2) else false
      }
    }
  }
  /* End of my function */
  
  /* The idea I wrote first is simply implemented. */
  def checkMetro(ipt: Metro): Boolean = {
    
      if (
        list_matching(list_station(ipt), list_area(ipt)) &&
        list_matching(list_area(ipt), list_station(ipt))
      ) {
        true 
      } else {
        false
      }
  }
}
