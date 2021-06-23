import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub135 {
  
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(mat: Metro): Boolean = {
    val _2 = {
      def checkStringInList(((li, st))) = {
        li match {
          case Nil() => { false }
          case Cons(a, remain) => {
            if (st == a) true else checkStringInList(remain, st)
          }
        }
      }
      val _3 = {
        def checkStationInArea(((listOfArea, subMat))) = {
          subMat match {
            case STATION(s) => { checkStringInList(listOfArea, s) }
            case CONNECT(m1, m2) => {
              checkStationInArea(listOfArea, m1) &&
              checkStationInArea(listOfArea, m2)
            }
            case AREA(a, m) => {
              val _6 = {
                val newlist = a :: listOfArea
                checkStationInArea(newlist, m)
              }
            }
          }
        }
        mat match {
          case STATION(s) => { false }
          case CONNECT(m1, m2) => { false }
          case AREA(a, m) => { checkStationInArea(List(a), m) }
        }
      }
    }
  }
  
  /* exercise test
  Printf.printf( "suposed to be true
  " );
  checkMetro( AREA( "a", STATION "a" ) );;
  checkMetro( AREA( "a", AREA("a", STATION "a") ) );;
  checkMetro( AREA( "a", AREA("b", CONNECT(STATION "a", STATION "b"))) );;
  checkMetro( AREA( "a", CONNECT(STATION "a", AREA("b", STATION "a"))) );;
  Printf.printf( "suposed to be false
  " );
  checkMetro( AREA( "a", STATION "b" ) );;
  checkMetro( AREA( "a", CONNECT(STATION "a", AREA("b", STATION "c"))) );;
  checkMetro( AREA( "a", AREA("b", CONNECT(STATION "a", STATION "c"))) );;
  exercise */
}