import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub170 {
  type Name = String
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def is_exist(((str, lst))) = {
    (str, lst) match {
      case (str, Nil()) => { false }
      case (str, Cons(h, t)) => { if (str == h) true else is_exist(str, t) }
    }
  }
  
  def checkMetro(mtr: Metro): Boolean = {
    val _2 = {
      def check_existing(((mtr, lst))) = {
        (mtr, lst) match {
          case (STATION(n), lst) => { is_exist(n, lst) }
          case (AREA(n, m), lst) => { check_existing(m, n :: lst) }
          case (CONNECT(m1, m2), lst) => {
            check_existing(m1, lst) && check_existing(m2, lst)
          }
        }
      }
      check_existing(mtr, Nil())
    }
  }
}