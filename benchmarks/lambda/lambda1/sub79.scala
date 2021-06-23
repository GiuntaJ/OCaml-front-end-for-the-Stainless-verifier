import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub79 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def check(((m, id_list))) = {
    m match {
      case STATION(id) => { id_list.contains(id) }
      case AREA(id, m1) => {
        
          if (
            id_list.contains(id)
          ) {
            check(m1, id_list) 
          } else {
            check(m1, id :: id_list)
          }
      }
      case CONNECT(m1, m2) => { check(m1, id_list) && check(m2, id_list) }
    }
  }
  def checkMetro(mtr: Metro): Boolean = { check(mtr, Nil()) }
}