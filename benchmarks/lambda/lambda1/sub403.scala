import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub403 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def list_check: (List[String], String) => Boolean = {
            case (l, s) =>
              {
                l match {
                  case Nil() => { false }
                  case Cons(head, tail) => {
                    if (head == s) true else list_check(tail, s)
                  }
                }
            }
          }
          val _5 = {
            def r_checkmetro: (Metro, List[String]) => Boolean = {
              case (m, l) =>
                {
                  m match {
                    case STATION(name) => { list_check(l, name) }
                    case AREA(name, metro) => { r_checkmetro(metro, name :: l) }
                    case CONNECT(metro1, metro2) => {
                      r_checkmetro(metro1, l) && r_checkmetro(metro2, l)
                    }
                  }
              }
            }
            r_checkmetro(m, Nil())
          }
        }
    }
  )
}