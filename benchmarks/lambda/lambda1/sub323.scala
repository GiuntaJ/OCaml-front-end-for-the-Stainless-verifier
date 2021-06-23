import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub323 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (a) =>
      {
        val _4 = {
          def checkM(((a, lst))) = {
            a match {
              case STATION(id) => { lst.exists(( (b) => { id == b } )) }
              case AREA(id, met) => { checkM(met, id :: lst) }
              case CONNECT(met1, met2) => {
                checkM(met1, lst) && checkM(met2, lst)
              }
            }
          }
          checkM(a, Nil())
        }
    }
  )
}