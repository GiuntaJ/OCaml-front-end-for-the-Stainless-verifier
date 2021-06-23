import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub368 {
  /* 2015-1478 Giyeon Kim HW 2 */
  
  /* Exercise 4 */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (imetro) =>
      {
        val _4 = {
          def checkMetroInner: (Metro, List[Name]) => Boolean = {
            case (imetro, ilist) =>
              {
                imetro match {
                  case STATION(lname) => { ilist.contains(lname) }
                  case AREA(lname, rmetro) => {
                    checkMetroInner(rmetro, lname :: ilist)
                  }
                  case CONNECT(lmetro, rmetro) => {
                    checkMetroInner(lmetro, ilist) &&
                    checkMetroInner(rmetro, ilist)
                  }
                }
            }
          }
          checkMetroInner(imetro, Nil())
        }
    }
  )
}
