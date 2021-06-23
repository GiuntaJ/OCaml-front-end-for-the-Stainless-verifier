import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub356 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def is_in_area(s, a) = {
        a match {
          case Nil() => { false }
          case Cons(h, t) => { if (h == s) true else is_in_area(s, t) }
        }
      }
      val _3 = {
        def aux(env) = {
          (
            x =>
              x match {
                case STATION(s) => { if (is_in_area(s, env)) true else false }
                case AREA(a, m) => { aux(a :: env, m) }
                case CONNECT(m1, m2) => { aux(env, m1) && aux(env, m2) }
              }
          )
        }
        aux(Nil(), metro)
      }
    }
  }
  
  
  /*
  let print_bool b = print_string (string_of_bool b) ; print_string "
  "
  
  
  let r1 = AREA("a", STATION "a")
  let r2 = AREA("a", AREA("a", STATION "a") )
  let r3 = AREA("a", AREA("b", CONNECT( STATION "a", STATION "b") ) )
  let r4 = AREA("a", CONNECT( STATION "a", AREA("b", STATION "a")))
  let f1 = AREA("a", STATION "b")
  let f2 = AREA("a", CONNECT( STATION "a", AREA("b", STATION "c") ) )
  let f3 = AREA("a", AREA("b", CONNECT( STATION "a", STATION "c")))
  let testcase = r1::r2::r3::r4::f1::f2::f3::[];;
  List.iter print_bool ( List.map checkMetro testcase )
  
  let a1 = AREA("a", STATION "a")
  let a2 = AREA("a", AREA("a", STATION "a"))
  let a3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
  let a4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
  let a5 = AREA("a", STATION "b")
  let a6 = AREA("a", CONNECT( STATION "a", AREA("b", STATION "c")))
  let a7 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
  let testcase2 = a1::a2::a3::a4::a5::a6::a7::[];;
  List.iter print_bool ( List.map checkMetro testcase2 )
  */
  
}
