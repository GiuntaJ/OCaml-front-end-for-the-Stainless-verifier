import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub401 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def listcheck(((str, namelist))) = {
        namelist match {
          case Nil() => { false }
          case Cons(a, lst) => { if (a == str) true else listcheck(str, lst) }
        }
      }
      val _3 = {
        def metrolistcheck(((met, arealist))) = {
          met match {
            case STATION(a) => { listcheck(a, arealist) }
            case AREA(a, m) => { metrolistcheck(m, a :: arealist) }
            case CONNECT(m1, m2) => {
              metrolistcheck(m1, arealist) && metrolistcheck(m2, arealist)
            }
          }
        }
        metrolistcheck(met, Nil())
      }
    }
  }
  
  
  
    
}