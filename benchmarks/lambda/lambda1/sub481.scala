import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub481 {
  /* HW2 Exercise 4 Check Metro Map */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  
  def checkMetro: Metro => Boolean = val _0 = {
    def checkList: (Metro, List[Name]) => Boolean = val _1 = {
      def isIdInList: (Name, List[Name]) => Boolean = {
        case (id_checking, id_list) =>
          {
            id_list match {
              case Nil() => { false }
              case Cons(head, tail) => {
                if (head == id_checking) true else isIdInList(id_checking, tail)
              }
            }
        }
      }
      {
        case (metro_checking, id_list) =>
          {
            metro_checking match {
              case STATION(id) => { isIdInList(id, id_list) }
              case AREA(id, metro) => { checkList(metro, id :: id_list) }
              case CONNECT(metro1, metro2) => {
                checkList(metro1, id_list) && checkList(metro2, id_list)
              }
            }
        }
      }
    }
    ( (metro_checking) => { checkList(metro_checking, Nil()) } )
  }
}
