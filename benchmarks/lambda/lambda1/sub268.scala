import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub268 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkMetroWithNames(((m, inArea))) = {
        m match {
          case STATION(n) => {
            val _5 = {
              def findNameInAreaList(((name, l))) = {
                l match {
                  case Nil() => { false }
                  case Cons(e, ll) => {
                    if (name == e) true else findNameInAreaList(name, ll)
                  }
                }
              }
              findNameInAreaList(n, inArea)
            }
          }
          case AREA(n, me) => { checkMetroWithNames(me, List(n) ++ inArea) }
          case CONNECT(m1, m2) => {
            
              if (
                checkMetroWithNames(m1, inArea) &&
                checkMetroWithNames(m2, inArea)
              ) {
                true 
              } else {
                false
              }
          }
        }
      }
      checkMetroWithNames(met, Nil())
    }
  }
}