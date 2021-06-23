import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub391 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def sub_check(m: Metro, id_list: List[Name]) = {
        m match {
          case STATION(id) => {
            val _5 = {
              def find(id, id_list) = {
                id_list match {
                  case Nil() => { false }
                  case Cons(hd, tl) => { if (hd == id) true else find(id, tl) }
                }
              }
              find(id, id_list)
            }
          }
          case AREA(id, m1) => { sub_check(m1, id :: id_list) }
          case CONNECT(m1, m2) => {
            sub_check(m1, id_list) && sub_check(m2, id_list)
          }
        }
      }
      sub_check(m, Nil())
    }
  }
}