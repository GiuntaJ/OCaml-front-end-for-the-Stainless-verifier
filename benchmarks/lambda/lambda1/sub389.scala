import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub389 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val sta_chk: (List[String], String) => Boolean = {
    case (s_list, str) => { s_list.contains(str) }
  }
  
  val checkMetro: Metro => Boolean = (
    (metr) =>
      {
        val _4 = {
          def check: (Metro, List[String]) => Boolean = {
            case (metro, sta_list) =>
              {
                metro match {
                  case AREA(sta, met) => { check(met, sta :: sta_list) }
                  case CONNECT(met1, met2) => {
                    check(met1, sta_list) && check(met2, sta_list)
                  }
                  case STATION(sta) => { sta_chk(sta_list, sta) }
                }
            }
          }
          check(metr, Nil())
        }
    }
  )
}