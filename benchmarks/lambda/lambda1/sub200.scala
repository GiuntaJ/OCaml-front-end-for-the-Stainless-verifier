import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub200 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def isinlst[A](mtr: A, lst: List[A]): Boolean = {
    (mtr, lst) match {
      case (_, Nil()) => { false }
      case (_, _) => { if (mtr == lst.head) true else isinlst(mtr, lst.tail) }
    }
  }
  
  
  def checkMetro1(mtr: Metro, lst: List[Name]): Boolean = {
    mtr match {
      case STATION(st) => { isinlst(st, lst) }
      case AREA(name, mt) => { checkMetro1(mt, name :: lst) }
      case CONNECT(m1, m2) => { checkMetro1(m1, lst) && checkMetro1(m2, lst) }
    }
  }
  
  def checkMetro(mtr: Metro): Boolean = { checkMetro1(mtr, Nil()) }
  
  /*
  let t1 = checkMetro(AREA("a", STATION "a"))
   let t2=checkMetro(AREA("a", AREA("a", STATION "a")))
   let t3=checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))
   let t4=checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))
   let t5=checkMetro(AREA("a", STATION "b"))
   let t6=checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))
   let t7=checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))
  
  */
}