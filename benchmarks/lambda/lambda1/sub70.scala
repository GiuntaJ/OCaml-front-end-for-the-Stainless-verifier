import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub70 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(_metro: Metro): Boolean = {
    val _2 = {
      def checkMetroRec(((_metro, _areaList))) = {
        _metro match {
          case STATION(_name) => { _areaList.contains(_name) }
          case AREA(_name, __metro) => {
            checkMetroRec(__metro, _name :: _areaList)
          }
          case CONNECT(_metro1, _metro2) => {
            checkMetroRec(_metro1, _areaList) &&
            checkMetroRec(_metro2, _areaList)
          }
        }
      }
      checkMetroRec(_metro, Nil())
    }
  }
}
