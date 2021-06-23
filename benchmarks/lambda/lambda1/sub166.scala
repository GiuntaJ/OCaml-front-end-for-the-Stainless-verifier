import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub166 {
  /* 4190.310 Programming Languages - Daegeun Lee <elnn@elnn.kr> */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (metro) =>
      {
        val _4 = {
          def contains(((lst, x))) = {
            lst match {
              case Nil() => { false }
              case Cons(h, t) => { if (h == x) true else contains(t, x) }
            }
          }
          val _5 = {
            def check(((metro, environ))) = {
              metro match {
                case STATION(name) => { contains(environ, name) }
                case AREA(name, metro) => { check(metro, name :: environ) }
                case CONNECT(m1, m2) => {
                  check(m1, environ) && check(m2, environ)
                }
              }
            }
            check(metro, Nil())
          }
        }
    }
  )
}
