import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub156 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def subCheckMetro(areaList: List[Name], mtr: Metro): Boolean = {
    mtr match {
      case STATION(name) => { areaList.contains(name) }
      case AREA(name, metro) => { subCheckMetro(areaList ++ List(name), metro) }
      case CONNECT(m1, m2) => {
        subCheckMetro(areaList, m1) && subCheckMetro(areaList, m2)
      }
    }
  }
  
  def checkMetro(mtr: Metro): Boolean = {
    mtr match {
      case STATION(_) => { false }
      case CONNECT(a, b) => { checkMetro(a) && checkMetro(b) }
      case AREA(name, metro) => { subCheckMetro(List(name), metro) }
    }
  }
}