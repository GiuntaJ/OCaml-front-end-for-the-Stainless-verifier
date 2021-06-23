import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub60 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def idCheck(id_list, m) = {
        val _5 = {
          def exists(f, l) = {
            l match {
              case Nil() => { false }
              case Cons(h, t) => { if (f == h) true else exists(f, t) }
            }
          }
          m match {
            case STATION(a) => { exists(a, id_list) }
            case AREA(id1, m1) => { idCheck(id1 :: id_list, m1) }
            case CONNECT(m1, m2) => {
              idCheck(id_list, m1) && idCheck(id_list, m2)
            }
          }
        }
      }
      m match {
        case AREA(name, metro) => { idCheck(List(name), metro) }
        case _ => { false }
      }
    }
  }
            
             
      
}