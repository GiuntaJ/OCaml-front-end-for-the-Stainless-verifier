import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub406 {
  /*
    Department : Electrical Engineering
    Student-Id : 2008-11923
    Name : HyeonIL Choi (최현일)
    Date: 2017-9-13
    Homework-# : 2-4
    Excercise-Name : Check metro map
  */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def checkMetroInContext(((metro, ctx))) = {
        metro match {
          case CONNECT(m1, m2) => {
            checkMetroInContext(m1, ctx) && checkMetroInContext(m2, ctx)
          }
          case AREA(name, m) => { checkMetroInContext(m, name :: ctx) }
          case STATION(name) => { ctx.contains(name) }
        }
      }
      checkMetroInContext(metro, Nil())
    }
  }
}
