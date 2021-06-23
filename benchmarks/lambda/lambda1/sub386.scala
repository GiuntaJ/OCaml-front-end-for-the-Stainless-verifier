import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub386 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (x) =>
      {
        val _4 = {
          def change: (Metro, List[String]) => List[(Metro, List[String])] = {
            case (x, l) =>
              {
                (x, l) match {
                  case (AREA(a, m), l) => { change(m, a :: l) }
                  case (STATION(a), l) => { List((STATION(a), l)) }
                  case (CONNECT(m1, m2), l) => { change(m1, l) ++(change(m2, l))
                  }
                }
            }
          }
          val _5 = {
            val eval: (Metro, List[String]) => Boolean = (
              (x) =>
                {
                  x match {
                    case (STATION(a), l) => { l.contains(a) }
                    case (AREA(a, m), l) => { false }
                    case (CONNECT(m1, m2), l) => { false }
                  }
              }
            )
            change(x, Nil()).forall(eval)
          }
        }
    }
  )
}