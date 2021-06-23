import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub446 {
  /*Lee Seok Jin 2013-11417 CSE hw2_4*/
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /* using anonymous function would be better as we learned in PP
  let compare((st, stan): 'a * 'a): bool = 
  */
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def rec_checkMetro(((met: Metro, area_list: List[String]))): Boolean = {
        met match {
          case STATION(s) => { area_list.exists(( (x) => { x == s } )) }
          case AREA(s, _m) => { rec_checkMetro(_m, s :: area_list) }
          case CONNECT(l, r) => {
            rec_checkMetro(l, area_list) && rec_checkMetro(r, area_list)
          }
        }
      }
      rec_checkMetro(m, Nil())
    }
  }
}