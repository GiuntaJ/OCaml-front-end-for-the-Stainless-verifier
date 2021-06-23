import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub123 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetro_(m, ((al, nl))) = {
        m match {
          case STATION(n) => { n :: nl.forall(( (x) => { al.contains(x) } )) }
          case AREA(n, m_) => { checkMetro_(m_, n :: al, nl) }
          case CONNECT(m_1, m_2) => {
            checkMetro_(m_1, al, nl) && checkMetro_(m_2, al, nl)
          }
        }
      }
      checkMetro_(m, Nil(), Nil())
    }
  }
  	
  /* TEST SET */
  /*
  checkMetro (STATION "a");;
  checkMetro (AREA("a", STATION "a"));;
  checkMetro (AREA("a", AREA("b", AREA ("c", CONNECT(STATION "c", STATION "c")))));;
  checkMetro (AREA("b", CONNECT(STATION "a", STATION "b")));;
  checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", AREA ("b", STATION "b")))));;
  checkMetro (AREA("a", AREA("a", STATION "a")));;
  checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))));;
  checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))));;
  checkMetro (AREA("a", STATION "b"));;
  checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))));;
  checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))));;
  */
}