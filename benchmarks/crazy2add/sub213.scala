import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub213 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((n, m))) = {
    n match {
      case NIL => { m }
      case ZERO(n_0) => {
        m match {
          case NIL => { NIL }
          case ZERO(m_0) => { ZERO(crazy2add(n_0, m_0)) }
          case ONE(m_0) => { ONE(crazy2add(n_0, m_0)) }
          case MONE(m_0) => { MONE(crazy2add(n_0, m_0)) }
        }
      }
      case ONE(n_0) => {
        m match {
          case NIL => { n }
          case ZERO(m_0) => { ONE(crazy2add(n_0, m_0)) }
          case ONE(m_0) => { ZERO(crazy2add(ONE(NIL), crazy2add(n_0, m_0))) }
          case MONE(m_0) => { ZERO(crazy2add(n_0, m_0)) }
        }
      }
      case MONE(n_0) => {
        m match {
          case NIL => { n }
          case ZERO(m_0) => { MONE(crazy2add(n_0, m_0)) }
          case ONE(m_0) => { ZERO(crazy2add(n_0, m_0)) }
          case MONE(m_0) => { ZERO(crazy2add(MONE(NIL), crazy2add(n_0, m_0))) }
        }
      }
    }
  }
}