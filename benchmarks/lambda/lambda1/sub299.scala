import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub299 {
  /* C:\Users\saigoy\Desktop\checkMetro.ml */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (metro) =>
      {
        val _4 = {
          def isProperStation(((name, areaList))) = {
            areaList match {
              case Nil() => { false }
              case Cons(hd, tl) => {
                if (hd == name) true else isProperStation(name, tl)
              }
            }
          }
          val _5 = {
            def checkMetro_Aux(((metro, areaList))) = {
              metro match {
                case STATION(n) => { isProperStation(n, areaList) }
                case AREA(n, m) => { checkMetro_Aux(m, n :: areaList) }
                case CONNECT(lm, rm) => {
                  checkMetro_Aux(lm, areaList) && checkMetro_Aux(rm, areaList)
                }
              }
            }
            checkMetro_Aux(metro, Nil())
          }
        }
    }
  )
}
