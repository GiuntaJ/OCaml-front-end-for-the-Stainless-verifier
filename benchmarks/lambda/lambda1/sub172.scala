import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub172 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(mtr: Metro): Boolean = {
    val _2 = {
      def check(((mtr, area))) = {
        mtr match {
          case STATION(nm) => { area.contains(nm) }
          case AREA(nm, _mtr) => {
            
              if (
                area.contains(nm)
              ) {
                check(_mtr, area) 
              } else {
                check(_mtr, nm :: area)
              }
          }
          case CONNECT(mtr1, mtr2) => { check(mtr1, area) && check(mtr2, area) }
        }
      }
      check(mtr, Nil())
    }
  }
}