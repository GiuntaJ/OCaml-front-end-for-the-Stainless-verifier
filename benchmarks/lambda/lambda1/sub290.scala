import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub290 {
  /* 생명과학부 / 2011-10915 / 신지민 / Homework 2-3 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkwithList: (List[Name], Metro) => Boolean = {
    case (mylist, m) =>
      {
        m match {
          case STATION(n) => { if (mylist.contains(n)) true else false }
          case AREA(n_in, m_in) => {
            val _2 = {
              val mylist = n_in :: mylist
              checkwithList(mylist, m_in)
            }
          }
          case CONNECT(m1, m2) => {
            
              if (
                checkwithList(mylist, m1) && checkwithList(mylist, m2)
              ) {
                true 
              } else {
                false
              }
          }
        }
    }
  }
  	
  
  val checkMetro: Metro => Boolean = ( (m) => { checkwithList(Nil(), m) } )
  
  /*
  let a = AREA("a", STATION "a")
  let b = AREA("a", AREA("a", STATION "a"))
  let c = AREA("a", AREA("b", CONNECT(STATION "a",STATION "b")))
  let d = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
  let e = AREA("a", STATION "b")
  let f = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
  let g = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
  
  let a1 = checkMetro a
  let b1 = checkMetro b
  let c1 = checkMetro c
  let d1 = checkMetro d
  let e1 = checkMetro e
  let f1 = checkMetro f
  let g1 = checkMetro g
  
  let _= print_endline(string_of_bool a1)
  let _= print_endline(string_of_bool b1)
  let _= print_endline(string_of_bool c1)
  let _= print_endline(string_of_bool d1)
  let _= print_endline(string_of_bool e1)
  let _= print_endline(string_of_bool f1)
  let _= print_endline(string_of_bool g1)
  */
  
}
