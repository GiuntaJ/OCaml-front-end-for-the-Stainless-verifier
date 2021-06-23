import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub234 {
  /* not tested */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def lst_check(l: List[Name], elt: Name): Boolean = {
    l match {
      case Cons(hd, tl) => { if (hd == elt) true else lst_check(tl, elt) }
      case Nil() => { false }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetro_sub(m: Metro, sl: List[Name]) = {
        m match {
          case STATION(n) => { lst_check(sl, n) }
          case AREA(n, sub_metro) => { checkMetro_sub(sub_metro, sl ++ List(n))
          }
          case CONNECT(m1, m2) => {
            checkMetro_sub(m1, sl) && checkMetro_sub(m2, sl)
          }
        }
      }
      checkMetro_sub(m, Nil())
    }
  }
}