import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub305 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def isIncluded(((lst, elem))) = {
    lst match {
      case Cons(head, tail) => {
        if (head == elem) true else isIncluded(tail, elem)
      }
      case _ => { false }
    }
  }
  		
  		
  def checkPartial(((areaNameLst, mtr))) = {
    mtr match {
      case STATION(stnName) => { isIncluded(areaNameLst, stnName) }
      case CONNECT(m1, m2) => {
        
          if (
            checkPartial(areaNameLst, m1) == false
          ) {
            false 
          } else {
            checkPartial(areaNameLst, m2)
          }
      }
      case AREA(subName, subMtr) => {
        checkPartial(subName :: areaNameLst, subMtr)
      }
    }
  }
  
  def checkMetro(mtr: Metro): Boolean = {
    mtr match {
      case STATION(_) => { false }
      case CONNECT(m1, m2) => {
        if (checkPartial(Nil(), m1) == false) false else checkPartial(Nil(), m2)
      }
      case AREA(n, m) => { checkPartial(Nil(), AREA(n, m)) }
    }
  }
}