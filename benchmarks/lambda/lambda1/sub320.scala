import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub320 {
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def checkMetroRec: (Metro, List[Name]) => Boolean = {
            case (m, l) =>
              {
                m match {
                  case STATION(id) => { l.exists(( (id_) => { id_ == id } )) }
                  case AREA(id, m_sub) => { checkMetroRec(m_sub, id :: l) }
                  case CONNECT(m_left, m_right) => {
                    checkMetroRec(m_left, l) && checkMetroRec(m_right, l)
                  }
                }
            }
          }
          checkMetroRec(m, Nil())
        }
    }
  )
}
